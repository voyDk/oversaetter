	.text	0x00400000
	.globl	main
	la	$28, _heap_
	jal	main
	ori	$2, $0, 10
	syscall
# 
main:
	addi	$29, $29, -12
	sw	$31, 8($29)
	sw	$17, 4($29)
	sw	$16, 0($29)
_block_begin__2_:
	ori	$2, $0, 2
# was:	ori	_exps__4_, 0, 2
# 	ori	2,_exps__4_,0
	jal	balloc
# was:	jal	balloc, 2
# 	ori	_assign__3_,2,0
	sb	$2, 0($16)
# was:	sb	_assign__3_, 0(a_1_)
# 	ori	0,_assign__3_,0
	ori	$2, $0, 0
# was:	ori	_exp__6_, 0, 0
	add	$2, $2, $16
# was:	add	_add__8_, _exp__6_, a_1_
	ori	$3, $0, 97
# was:	ori	_assign__5_, 0, 97
	sb	$3, 0($2)
# was:	sb	_assign__5_, 0(_add__8_)
# 	ori	0,_assign__5_,0
	ori	$2, $0, 1
# was:	ori	_exp__11_, 0, 1
	add	$2, $2, $16
# was:	add	_add__13_, _exp__11_, a_1_
	ori	$3, $0, 98
# was:	ori	_assign__10_, 0, 98
	sb	$3, 0($2)
# was:	sb	_assign__10_, 0(_add__13_)
# 	ori	0,_assign__10_,0
	ori	$2, $0, 1
# was:	ori	_exp__20_, 0, 1
	add	$2, $2, $16
# was:	add	_add__22_, _exp__20_, a_1_
	lb	$4, 0($2)
# was:	lb	_equal1__17_, 0(_add__22_)
	ori	$3, $0, 98
# was:	ori	_equal2__18_, 0, 98
	ori	$2, $0, 0
# was:	ori	_if__15_, 0, 0
	bne	$4, $3, _equal_branch__19_
# was:	bne	_equal1__17_, _equal2__18_, _equal_branch__19_
	ori	$2, $0, 1
# was:	ori	_if__15_, 0, 1
_equal_branch__19_:
	beq	$2, $0, _endif__16_
# was:	beq	_if__15_, 0, _endif__16_
_block_begin__24_:
	ori	$2, $0, 1
# was:	ori	_exps__25_, 0, 1
# 	ori	2,_exps__25_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 1
# was:	ori	_return__26_, 0, 1
# 	ori	2,_return__26_,0
	j	main_exit
_block_exit__24_:
_endif__16_:
	ori	$2, $0, 0
# was:	ori	_exps__27_, 0, 0
# 	ori	2,_exps__27_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 0
# was:	ori	_return__28_, 0, 0
# 	ori	2,_return__28_,0
	j	main_exit
_block_exit__2_:
main_exit:
	lw	$17, 4($29)
	lw	$16, 0($29)
	lw	$31, 8($29)
	addi	$29, $29, 12
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
	ori	$4, $2, 0
	sll	$4, $4, 2
	ori	$2, $0, 9
	syscall
	ori	$16, $2, 0
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
	ori	$16, $2, 0
	lw	$4, 0($29)
	addi	$29, $29, 4
	jr	$31
	.data	
	.align	2
_cr_:
	.asciiz	"\n"
	.align	2
_heap_:
	.space	100000
