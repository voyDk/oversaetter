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
_block_begin__3_:
	ori	$2, $0, 2
# was:	ori	_exps__5_, 0, 2
# 	ori	2,_exps__5_,0
	jal	balloc
# was:	jal	balloc, 2
# 	ori	_assign__4_,2,0
	sb	$2, 0($16)
# was:	sb	_assign__4_, 0(a_1_)
# 	ori	0,_assign__4_,0
	ori	$2, $0, 0
# was:	ori	_exp__7_, 0, 0
	add	$2, $2, $16
# was:	add	_add__9_, _exp__7_, a_1_
	ori	$3, $0, 97
# was:	ori	_assign__6_, 0, 97
	sb	$3, 0($2)
# was:	sb	_assign__6_, 0(_add__9_)
# 	ori	0,_assign__6_,0
	ori	$2, $0, 1
# was:	ori	_exp__12_, 0, 1
	add	$2, $2, $16
# was:	add	_add__14_, _exp__12_, a_1_
	ori	$3, $0, 98
# was:	ori	_assign__11_, 0, 98
	sb	$3, 0($2)
# was:	sb	_assign__11_, 0(_add__14_)
# 	ori	0,_assign__11_,0
	ori	$2, $0, 0
# was:	ori	_exp__20_, 0, 0
	add	$2, $2, $16
# was:	add	_add__22_, _exp__20_, a_1_
	lb	$2, 0($2)
# was:	lb	_equal1__17_, 0(_add__22_)
	ori	$3, $0, 0
# was:	ori	_exp__24_, 0, 0
	add	$3, $3, $16
# was:	add	_add__26_, _exp__24_, a_1_
	lb	$3, 0($3)
# was:	lb	_equal2__18_, 0(_add__26_)
	ori	$4, $0, 0
# was:	ori	_assign__16_, 0, 0
	bne	$2, $3, _equal_branch__19_
# was:	bne	_equal1__17_, _equal2__18_, _equal_branch__19_
	ori	$4, $0, 1
# was:	ori	_assign__16_, 0, 1
_equal_branch__19_:
# 	ori	b_2_,_assign__16_,0
# 	ori	0,_assign__16_,0
	ori	$2, $4, 0
# was:	ori	_exps__28_, b_2_, 0
# 	ori	2,_exps__28_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 0
# was:	ori	_return__29_, 0, 0
# 	ori	2,_return__29_,0
	j	main_exit
_block_exit__3_:
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
