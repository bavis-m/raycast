// RaycastRuntime.cpp : Defines the entry point for the console application.
//
#include "RaycastRuntime.h"

// SDL stuff
SDL_Window* window;
SDL_GLContext context;

// rendering
GLuint vbo;
GLuint vao;

// shaders
GLuint vShader;
GLuint fShader;
GLuint program;

extern "C" bool __cdecl init()
{
	SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, 1);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);

	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 32);

	if (SDL_Init(SDL_INIT_EVERYTHING) < 0)
	{
		cout << SDL_GetError() << std::endl;
		return false;
	}

	if (!(window = SDL_CreateWindow("Raycast2", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN)))
	{
		cout << SDL_GetError() << std::endl;
		SDL_Quit();
		return false;
	}

	if (!(context = SDL_GL_CreateContext(window)))
	{
		cout << SDL_GetError() << std::endl;
		SDL_Quit();
		return false;
	}


	glewExperimental = GL_TRUE;
	GLenum glewOK = glewInit();
	if (glewOK != GLEW_OK)
	{
		cout << glewGetErrorString(glewOK) << std::endl;
		SDL_Quit();
		return false;
	}

	glGenBuffers(1, &vbo);
	glGenVertexArrays(1, &vao);

	glBindVertexArray(vao);
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glBindVertexArray(0);

	vShader = glCreateShader(GL_VERTEX_SHADER);
	if (!compileShader(vShader, "resources/shaders/vshader.glsl")) return false;

	fShader = glCreateShader(GL_FRAGMENT_SHADER);
	if (!compileShader(fShader, "resources/shaders/fshader.glsl")) return false;

	program = glCreateProgram();
	glAttachShader(program, vShader);
	glAttachShader(program, fShader);

	if (!linkProgram(program, "main_program")) return false;
	
	ilInit();
	ilOriginFunc(IL_ORIGIN_LOWER_LEFT);
	ilEnable(IL_ORIGIN_SET);
	iluInit();

	SDL_GL_SetSwapInterval(true);

	glClearColor(1, 1, 1, 1);

	return true;
}

typedef struct _vertex {
	float x;
	float y;
	float u;
	float v;
} vertex;

typedef struct _vertexStorage {
	GLuint tex;
	int dataSize;
	int index;
	int total;
	vertex* vertexData;
	wstring* texName;
} vertexStorage;

vector<vertexStorage*> vertexStoragePerTexture;

extern "C" int __cdecl loadTexture(wchar_t* file)
{
	ILuint img = ilGenImage();
	ilBindImage(img);

	ILboolean success = ilLoadImage(file);
	if (!success)
	{
		ILenum error = ilGetError();
		wcout << L"IL error: " << iluErrorString(error) << std::endl;
		return -1;
	}

	GLuint tex;
	glGenTextures(1, &tex);

	int w = ilGetInteger(IL_IMAGE_WIDTH);
	int h = ilGetInteger(IL_IMAGE_HEIGHT);

	GLuint oldBinding;
	glGetIntegerv(GL_TEXTURE_BINDING_2D, reinterpret_cast<GLint*>(&oldBinding));

	glBindTexture(GL_TEXTURE_2D, tex);

	glTexStorage2D(GL_TEXTURE_2D, 1, GL_RGBA8, w, h);

	ILuint format = ilGetInteger(IL_IMAGE_FORMAT);
	ILuint type = ilGetInteger(IL_IMAGE_TYPE);

	if ((format != IL_RGBA && format != IL_RGB) ||
		type != IL_UNSIGNED_BYTE)
	{
		cout << "Invalid tex format" << std::endl;

		glBindTexture(GL_TEXTURE_2D, oldBinding);

		return -1;
	}

	GLenum glFormat = format == IL_RGBA ? GL_RGBA : GL_RGB;

	glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, w, h, glFormat, GL_UNSIGNED_BYTE, ilGetData());

	bool ok = checkGLErrors();
	if (!ok)
	{
		glBindTexture(GL_TEXTURE_2D, oldBinding);
		return -1;
	}

	glBindTexture(GL_TEXTURE_2D, oldBinding);

	int index = (int)vertexStoragePerTexture.size();

	vertexStorage* storage = (vertexStorage*)malloc(sizeof(vertexStorage));

	int dataSize = 1024;

	storage->tex = tex;
	storage->texName = new wstring(file);
	storage->dataSize = dataSize;
	storage->vertexData = (vertex*)malloc(sizeof(vertex) * dataSize * 6);

	vertexStoragePerTexture.push_back(storage);

	wcout << "Loaded texture " << file << " (" << index << ")" << std::endl;

	return index;
}

void resizeVertexStorage(vertexStorage* storage, int newSize)
{
	storage->vertexData = (vertex*)realloc(storage->vertexData, sizeof(vertex) * newSize * 6);
	storage->dataSize = newSize;
}

bool quit = false;

BOOL WINAPI CtrlHandler(DWORD ctrlType)
{
	switch (ctrlType)
	{
	case CTRL_C_EVENT:
		quit = true;
		return TRUE;
	}
	return FALSE;
}

extern "C" void __cdecl run(updateDataFunc updateFunc)
{
	SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);

	SDL_SetRelativeMouseMode(SDL_TRUE);
	SDL_ShowCursor(0);

	SDL_Event e;

	Uint32 lastTicks;

	float frameRate = 60.0f;
	float invFrameRate = 1.0f / frameRate;
	int invFrameRateMS = (int)round(invFrameRate * 1000);

	renderParams* params = (renderParams*)malloc(sizeof(renderParams));
	updateFrame* update = (updateFrame*)malloc(sizeof(updateFrame));

	int* keysPressed = new int[128];
	int* keysReleased = new int[128];

	update->keysPressed = keysPressed;
	update->keysReleased = keysReleased;

	while (!quit)
	{
		lastTicks = SDL_GetTicks();

		update->numKeysPressed = 0;
		update->numKeysReleased = 0;

		while (SDL_PollEvent(&e))
		{
			switch (e.type)
			{
			case SDL_QUIT:
				quit = true;
				break;			
			case SDL_KEYDOWN:
				if (e.key.keysym.sym == SDLK_ESCAPE)
				{
					quit = true;
				}
				else
				{
					if (e.key.repeat == 0)
					{
						update->keysPressed[update->numKeysPressed++] = (int)e.key.keysym.sym;
						//cout << "PRESSED " << e.key.keysym.sym << std::endl;
					}
				}
				break;
			case SDL_KEYUP:
				if (e.key.repeat == 0)
				{
					update->keysReleased[update->numKeysReleased++] = (int)e.key.keysym.sym;
					//cout << "RELEASED " << e.key.keysym.sym << std::endl;
				}
				break;
			}
		}

		//quit = true;

		int windowW, windowH;
		SDL_GetWindowSize(window, &windowW, &windowH);

		params->width = windowW;
		params->height = windowH;

		SDL_GetRelativeMouseState(&update->mouseRelX, &update->mouseRelY);

		update->time = invFrameRate;
		engineFrame* frame = updateFunc(params, update);

		Uint32 updateTicks = SDL_GetTicks();

		//cout << "Updated state numColumnSections: " << frame->numColumnSections << std::endl;

		glClear(GL_COLOR_BUFFER_BIT);

		glActiveTexture(GL_TEXTURE0);		

		glUseProgram(program);

		glBindAttribLocation(program, 0, "pos");
		glBindAttribLocation(program, 1, "uv");

		mat4 translateM = translate(mat4(), vec3(-1.0, -1.0, 0.0));

		mat4 scaleM = scale(mat4(), vec3(2.0 / windowW, 2.0 / windowH, 1.0));
		
		mat4 viewM = translateM * scaleM;

		float* ptr = value_ptr(viewM);
		GLint viewLoc = glGetUniformLocation(program, "view");
		glUniformMatrix4fv(viewLoc, 1, GL_FALSE, ptr);
		
		for (int tex = 0; tex < vertexStoragePerTexture.size(); tex++)
		{
			vertexStoragePerTexture[tex]->index = 0;
			vertexStoragePerTexture[tex]->total = 0;
		}

		for (int i = 0; i < frame->numColumnSections; i++)
		{
			columnSection section = frame->columnMemory[i];

			if (section.tex == -1) continue;

			vertexStorage* storage = vertexStoragePerTexture[section.tex];

			if (storage->total >= storage->dataSize)
			{
				resizeVertexStorage(storage, storage->dataSize * 2);
			}

			vertex* vertexData = storage->vertexData;

			int indexBase = storage->index * 6;

			vertexData[indexBase + 0].x = (float)section.column;
			vertexData[indexBase + 0].y = (float)section.startY;
			vertexData[indexBase + 0].u = section.startU;
			vertexData[indexBase + 0].v = section.startV;

			vertexData[indexBase + 1].x = (float)section.column + 1;
			vertexData[indexBase + 1].y = (float)section.startY;
			vertexData[indexBase + 1].u = section.startU;
			vertexData[indexBase + 1].v = section.startV;

			vertexData[indexBase + 2].x = (float)section.column + 1;
			vertexData[indexBase + 2].y = (float)section.endY + 1;
			vertexData[indexBase + 2].u = section.endU;
			vertexData[indexBase + 2].v = section.endV;

			vertexData[indexBase + 3].x = (float)section.column;
			vertexData[indexBase + 3].y = (float)section.startY;
			vertexData[indexBase + 3].u = section.startU;
			vertexData[indexBase + 3].v = section.startV;

			vertexData[indexBase + 4].x = (float)section.column + 1;
			vertexData[indexBase + 4].y = (float)section.endY + 1;
			vertexData[indexBase + 4].u = section.endU;
			vertexData[indexBase + 4].v = section.endV;

			vertexData[indexBase + 5].x = (float)section.column;
			vertexData[indexBase + 5].y = (float)section.endY + 1;
			vertexData[indexBase + 5].u = section.endU;
			vertexData[indexBase + 5].v = section.endV;

			storage->index++;
			storage->total++;
		}

		Uint32 memTicks = SDL_GetTicks();

		Uint32 drawStartTicks = SDL_GetTicks();

		for (int tex = 0; tex < vertexStoragePerTexture.size(); tex++)
		{
			vertexStorage* storage = vertexStoragePerTexture[tex];
			glBindTexture(GL_TEXTURE_2D, storage->tex);

			glBindVertexArray(vao);

			glBindBuffer(GL_ARRAY_BUFFER, vbo);
			glBufferData(GL_ARRAY_BUFFER, sizeof(vertex) * 6 * storage->total, storage->vertexData, GL_DYNAMIC_DRAW);

			glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 4, 0);
			glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 4, (void*)(sizeof(float) * 2));

			GLint texLoc = glGetUniformLocation(program, "tex");

			glUniform1i(texLoc, 0);

			glBindVertexArray(vao);

			glDrawArrays(GL_TRIANGLES, 0, storage->total * 6);

			Uint32 drawTicks = SDL_GetTicks();

			wcout << "  Render " << *storage->texName << " " << (drawTicks - drawStartTicks) << "  (" << storage->total << ")" << std::endl;
			drawStartTicks = drawTicks;
		}

		//checkGLErrors();

		SDL_GL_SwapWindow(window);

		Uint32 now = SDL_GetTicks();
		Uint32 delta = now - lastTicks;

		int waitTime = max(0, invFrameRateMS - (int)delta);
		cout << "Delay " << waitTime << "   " << (updateTicks - lastTicks) << "   " << (memTicks - updateTicks) << std::endl;

		SDL_Delay(waitTime);
	}

	for (int tex = 0; tex < vertexStoragePerTexture.size(); tex++)
	{
		free(vertexStoragePerTexture[tex]->vertexData);
	}

	SDL_GL_DeleteContext(context);
	SDL_DestroyWindow(window);
	SDL_Quit();

	SDL_ShowCursor(1);
	SDL_SetRelativeMouseMode(SDL_FALSE);

	SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, FALSE);
}

bool compileShader(GLint shader, char* filename)
{
	char* source = readFile(filename);

	if (source == NULL)
	{
		cout << "Couldn't open " << filename << std::endl;
		return false;
	}

	cout << "Building (" << strlen(source) << "):" << std::endl;
	cout << source << std::endl;

	glShaderSource(shader, 1, &source, NULL);
	glCompileShader(shader);

	GLint compileStatus;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &compileStatus);

	if (compileStatus == GL_FALSE)
	{
		GLint logLength;
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &logLength);

		GLchar* log = new char[logLength];
		glGetShaderInfoLog(shader, logLength, NULL, log);

		cout << "Couldn't compile shader " << filename << std::endl << log << std::endl;

		return false;
	}

	return checkGLErrors();
}

bool linkProgram(GLuint p, char* name)
{
	glLinkProgram(p);

	GLint isLinked = 0;
	glGetProgramiv(p, GL_LINK_STATUS, &isLinked);

	if (isLinked == GL_FALSE)
	{
		GLint logLength;
		glGetProgramiv(p, GL_INFO_LOG_LENGTH, &logLength);

		GLchar* log = new char[logLength];
		glGetProgramInfoLog(p, logLength, NULL, log);

		cout << "Couldn't compile program " << name << std::endl << log << std::endl;

		return false;
	}

	return true;
}

bool checkGLErrors(bool print)
{
	bool ok = true;
	GLenum error;
	while ((error = glGetError()) != GL_NO_ERROR)
	{
		ok = false;
		if (print)
		{
			cout << "OpenGL error: " << gluErrorString(error) << std::endl;
		}
	}
	return ok;
}

char* readFile(char* name)
{
	ifstream fileStream;
	fileStream.open(name);
	if (!fileStream.is_open()) return NULL;
	fileStream.seekg(0, ios_base::end);
	int len = (int)fileStream.tellg();
	fileStream.seekg(0, ios_base::beg);
	char* text = new char[len + 1];
	fileStream.read(text, len);
	text[fileStream.gcount()] = 0;
	return text;
}