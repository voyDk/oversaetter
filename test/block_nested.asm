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
	jal	f
# was:	jal	f, 
# 	ori	_assign__3_,2,0
# 	ori	a_1_,_assign__3_,0
# 	ori	0,_assign__3_,0
# 	ori	_exps__4_,a_1_,0
# 	ori	2,_exps__4_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 0
# was:	ori	_return__5_, 0, 0
# 	ori	2,_return__5_,0
	j	main_exit
_block_exit__2_:
main_exit:
	lw	$16, 0($29)
	lw	$31, 4($29)
	addi	$29, $29, 8
	jr	$31
# 
f:
	addi	$29, $29, -8
	sw	$31, 4($29)
	sw	$16, 0($29)
_block_begin__8_:
	ori	$4, $0, 1
# was:	ori	_assign__9_, 0, 1
# 	ori	b_6_,_assign__9_,0
# 	ori	0,_assign__9_,0
	ori	$3, $0, 1
# was:	ori	_assign__10_, 0, 1
# 	ori	c_7_,_assign__10_,0
# 	ori	0,_assign__10_,0
_while__11_:
# 	ori	_equal1__13_,b_6_,0
# 	ori	_equal2__14_,c_7_,0
	ori	$2, $0, 0
# was:	ori	_while__11_, 0, 0
	bne	$4, $3, _equal_branch__15_
# was:	bne	_equal1__13_, _equal2__14_, _equal_branch__15_
	ori	$2, $0, 1
# was:	ori	_while__11_, 0, 1
_equal_branch__15_:
	beq	$2, $0, _endwhile__12_
# was:	beq	_while__11_, 0, _endwhile__12_
_block_begin__16_:
# 	ori	_plus1__18_,c_7_,0
	ori	$2, $0, 1
# was:	ori	_plus2__19_, 0, 1
	add	$3, $3, $2
# was:	add	_assign__17_, _plus1__18_, _plus2__19_
# 	ori	c_7_,_assign__17_,0
# 	ori	0,_assign__17_,0
_block_exit__16_:
	j	_while__11_
_endwhile__12_:
	ori	$2, $3, 0
# was:	ori	_return__20_, c_7_, 0
# 	ori	2,_return__20_,0
	j	f_exit
_block_exit__8_:
f_exit:
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
