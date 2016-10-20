INCLUDELIB OLDNAMES

_TEXT SEGMENT

PUBLIC	updateCallStub
updateCallStub PROC FRAME
	sub rsp, 200
.allocstack 200
	movdqa xmmword ptr [rsp+32], xmm6
.savexmm128 xmm6, 32
	movdqa xmmword ptr [rsp+48], xmm7
.savexmm128 xmm7, 48
	movdqa xmmword ptr [rsp+64], xmm8
.savexmm128 xmm8, 64
	movdqa xmmword ptr [rsp+80], xmm9
.savexmm128 xmm9, 80
	movdqa xmmword ptr [rsp+96], xmm10
.savexmm128 xmm10, 96
	movdqa xmmword ptr [rsp+112], xmm11
.savexmm128 xmm11, 112
	movdqa xmmword ptr [rsp+128], xmm12
.savexmm128 xmm12, 128
	movdqa xmmword ptr [rsp+144], xmm13
.savexmm128 xmm13, 144
	movdqa xmmword ptr [rsp+160], xmm14
.savexmm128 xmm14, 160
	movdqa xmmword ptr [rsp+176], xmm15
.savexmm128 xmm15, 176
.endprolog

	mov	r10, rcx

	mov	rcx, rdx
	mov	rdx, r8
	call	r10

	movdqa xmm6, xmmword ptr [rsp+32]
	movdqa xmm7, xmmword ptr [rsp+48]
	movdqa xmm8, xmmword ptr [rsp+64]
	movdqa xmm9, xmmword ptr [rsp+80]
	movdqa xmm10, xmmword ptr [rsp+96]
	movdqa xmm11, xmmword ptr [rsp+112]
	movdqa xmm12, xmmword ptr [rsp+128]
	movdqa xmm13, xmmword ptr [rsp+144]
	movdqa xmm14, xmmword ptr [rsp+160]
	movdqa xmm15, xmmword ptr [rsp+176]
	add	rsp, 200
	ret
updateCallStub ENDP

_TEXT	ENDS

END
