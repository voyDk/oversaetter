	.text	0x00400000
	.globl	main
	la	$28, _heap_
	jal	main
	ori	$2, $0, 10
	syscall
# 
main:
	addi	$29, $29, -8
	sw	$31, 4($29)
	sw	$16, 0($29)
_block_begin__2_:
	ori	$3, $0, 97
# was:	ori	_assign__3_, 0, 97
# 	ori	c_1_,_assign__3_,0
# 	ori	0,_assign__3_,0
# 	ori	_equal1__6_,c_1_,0
	ori	$2, $0, 97
# was:	ori	_equal2__7_, 0, 97
	ori	$4, $0, 0
# was:	ori	_if__4_, 0, 0
	bne	$3, $2, _equal_branch__8_
# was:	bne	_equal1__6_, _equal2__7_, _equal_branch__8_
	ori	$4, $0, 1
# was:	ori	_if__4_, 0, 1
_equal_branch__8_:
	beq	$4, $0, _endif__5_
# was:	beq	_if__4_, 0, _endif__5_
_block_begin__9_:
	ori	$2, $0, 1
# was:	ori	_exps__10_, 0, 1
# 	ori	2,_exps__10_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 1
# was:	ori	_return__11_, 0, 1
# 	ori	2,_return__11_,0
	j	main_exit
_block_exit__9_:
_endif__5_:
	ori	$2, $0, 0
# was:	ori	_exps__12_, 0, 0
# 	ori	2,_exps__12_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 0
# was:	ori	_return__13_, 0, 0
# 	ori	2,_return__13_,0
	j	main_exit
_block_exit__2_:
main_exit:
	lw	$16, 0($29)
	lw	$31, 4($29)
	addi	$29, $29, 8
	jr	$31
putint:
	addi	$29, $29, -8
	sw	$2, 0($29)
	sw	$4, 4($29)
	ori	$4, $2, 0
	ori	$2, $0, 1
	syscall
	ori	$2, $0, 4
	la	$4, _cr_
	syscall
	lw	$2, 0($29)
	lw	$4, 4($29)
	addi	$29, $29, 8
	jr	$31
getint:
	ori	$2, $0, 5
	syscall
	jr	$31
walloc:
	addi	$29, $29, -4
	sw	$4, 0($29)
	ori	$4, $2, 0
	sll	$4, $4, 2
	ori	$2, $0, 9
	syscall
	add	$16, $2, $0
	lw	$4, 0($29)
	addi	$29, $29, 4
	jr	$31
balloc:
	addi	$29, $29, -4
	sw	$4, 0($29)
	ori	$4, $2, 0
	ori	$8, $4, 0
_remaind_:
	addi	$9, $0, 1
	slt	$10, $8, $9
	bne	$10, $0, _remaind_exit
	addi	$11, $11, 1
	addi	$12, $0, 4
	sub	$8, $8, $12
	j	_remaind_
_remaind_exit:
	sll	$11, $11, 2
	ori	$2, $0, 9
	syscall
	jr	$31
	.data	
	.align	2
_cr_:
	.asciiz	"\n"
	.align	2
_heap_:
	.space	100000
