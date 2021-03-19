.data
msg_4:
	.word 92
	.ascii "[31m(Runtime Error): Trying to store too small/large result in a 4-byte signed-integer[0m\0"
msg_5:
	.word 60
	.ascii "[31m(Runtime Error): Trying to access a negative index[0m\0"
msg_6:
	.word 72
	.ascii "[31m(Runtime Error): Trying to access an index out of array bounds[0m\0"
msg_7:
	.word 4
	.ascii "%d\n\0"
.text
.global main
f_inc_0:
	PUSH {LR}
	ADD r4, sp, #4
	LDR r4, [r4]
	LDR r5, =1
	ADDS r4, r4, r5
	MOV r0, #0
	LDR r1, =8
	BLVS p_throw_overflow_error
	MOV r0, r4
	POP {PC}
	POP {PC}
	.ltorg
f_dec_0:
	PUSH {LR}
	ADD r4, sp, #4
	LDR r4, [r4]
	LDR r5, =1
	SUBS r4, r4, r5
	MOV r0, #0
	LDR r1, =8
	BLVS p_throw_overflow_error
	MOV r0, r4
	POP {PC}
	POP {PC}
	.ltorg
f_id_0:
	PUSH {LR}
	ADD r4, sp, #4
	LDR r4, [r4]
	MOV r0, r4
	POP {PC}
	POP {PC}
	.ltorg
main:
	PUSH {LR}
	LDR r4, =10
	SUB sp, sp, #4
	STR r4, [sp]
	LDR r0, =16
	BL malloc
	MOV r5, #3
	STR r5, [r0]
	MOV r4, r0
	LDR r5, =f_inc_0
	STR r5, [r4, #4]
	LDR r5, =f_id_0
	STR r5, [r4, #8]
	LDR r5, =f_dec_0
	STR r5, [r4, #12]
	SUB sp, sp, #4
	STR r4, [sp]
	LDR r4, =0
	SUB sp, sp, #4
	STR r4, [sp]
	B L8
L9:
	ADD r4, sp, #4
	MOV r5, sp
	LDR r5, [r5]
	LDR r4, [r4]
	MOV r0, r5
	MOV r1, r4
	MOV r2, #0
	LDR r3, =16
	BL p_check_array_bounds
	ADD r4, r4, #4
	ADD r4, r4, r5, LSL #2
	LDR r4, [r4]
	SUB sp, sp, #4
	STR r4, [sp]
	ADD r4, sp, #12
	LDR r4, [r4]
	SUB sp, sp, #4
	STR r4, [sp]
	ADD r4, sp, #4
	LDR r4, [r4]
	BLX r4
	ADD sp, sp, #4
	MOV r4, r0
	SUB sp, sp, #4
	STR r4, [sp]
	MOV r4, sp
	LDR r4, [r4]
	LDR r0, =msg_7
	ADD r0, r0, #4
	MOV r1, r4
	BL printf
	ADD sp, sp, #8
L10:
	MOV r4, sp
	LDR r4, [r4]
	LDR r5, =1
	ADDS r4, r4, r5
	MOV r0, #0
	LDR r1, =16
	BLVS p_throw_overflow_error
	MOV r5, sp
	STR r4, [r5]
L8:
	MOV r4, sp
	LDR r4, [r4]
	ADD r5, sp, #4
	LDR r5, [r5]
	LDR r5, [r5]
	CMP r4, r5
	MOVLT r4, #1
	MOVGE r4, #0
	CMP r4, #1
	BEQ L9
L11:
	ADD sp, sp, #4
	ADD sp, sp, #8
	LDR r0, =0
	POP {PC}
	.ltorg
p_throw_overflow_error:
	PUSH {LR}
	MOV r2, r1
	MOV r1, r0
	LDR r0, =msg_4
	BL p_throw_runtime_error
	POP {PC}
p_check_array_bounds:
	PUSH {LR}
	CMP r0, #0
	LDRLT r0, =msg_5
	PUSH {r1}
	PUSH {r2}
	MOV r1, r2
	MOV r2, r3
	BLLT p_throw_runtime_error
	POP {r2}
	POP {r1}
	LDR r1, [r1]
	CMP r0, r1
	LDRCS r0, =msg_6
	MOV r1, r2
	MOV r2, r3
	BLCS p_throw_runtime_error
	POP {PC}
p_throw_runtime_error:
	ADD r0, r0, #4
	CMP r1, #0
	ADD sp, sp, r2
	ADD sp, sp, #4
	BXNE r1
	SUB sp, sp, #4
	SUB sp, sp, r2
	BL printf
	MOV r0, #255
	BL exit
