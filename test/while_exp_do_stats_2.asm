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
	ori	$3, $0, 1
# was:	ori	_assign__3_, 0, 1
# 	ori	a_1_,_assign__3_,0
# 	ori	0,_assign__3_,0
_while__4_:
# 	ori	_less1__6_,a_1_,0
	ori	$2, $0, 1
# was:	ori	_less2__7_, 0, 1
	slt	$2, $3, $2
# was:	slt	_while__4_, _less1__6_, _less2__7_
	beq	$2, $0, _endwhile__5_
# was:	beq	_while__4_, 0, _endwhile__5_
_block_begin__8_:
# 	ori	_plus1__10_,a_1_,0
	ori	$2, $0, 1
# was:	ori	_plus2__11_, 0, 1
	add	$3, $3, $2
# was:	add	_assign__9_, _plus1__10_, _plus2__11_
# 	ori	a_1_,_assign__9_,0
# 	ori	0,_assign__9_,0
_block_exit__8_:
	j	_while__4_
_endwhile__5_:
	ori	$2, $3, 0
# was:	ori	_exps__12_, a_1_, 0
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
