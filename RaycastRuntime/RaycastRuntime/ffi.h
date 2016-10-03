#pragma once

#include "HaskellStructs.h"

typedef engineFrame* (*updateDataFunc)(renderParams*, updateFrame*);

#ifdef __cplusplus
extern "C"
{
#endif

	typedef void(*HSFunPtr)(void);

#ifdef __cplusplus
}
#endif