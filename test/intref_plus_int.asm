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
	ori	$2, $0, 5
# was:	ori	_exps__5_, 0, 5
# 	ori	2,_exps__5_,0
	jal	walloc
# was:	jal	walloc, 2
# 	ori	_assign__4_,2,0
	sw	$2, 0($16)
# was:	sw	_assign__4_, 0(r_2_)
# 	ori	0,_assign__4_,0
	ori	$2, $0, 0
# was:	ori	_exp__7_, 0, 0
	sll	$2, $2, 2
# was:	sll	_offset__8_, _exp__7_, 2
	add	$2, $2, $16
# was:	add	_add__9_, _offset__8_, r_2_
	ori	$3, $0, 2
# was:	ori	_assign__6_, 0, 2
	sw	$3, 0($2)
# was:	sw	_assign__6_, 0(_add__9_)
# 	ori	0,_assign__6_,0
	ori	$2, $0, 1
# was:	ori	_exp__12_, 0, 1
	sll	$2, $2, 2
# was:	sll	_offset__13_, _exp__12_, 2
	add	$2, $2, $16
# was:	add	_add__14_, _offset__13_, r_2_
	ori	$3, $0, 4
# was:	ori	_assign__11_, 0, 4
	sw	$3, 0($2)
# was:	sw	_assign__11_, 0(_add__14_)
# 	ori	0,_assign__11_,0
	ori	$2, $0, 2
# was:	ori	_exp__17_, 0, 2
	sll	$2, $2, 2
# was:	sll	_offset__18_, _exp__17_, 2
	add	$2, $2, $16
# was:	add	_add__19_, _offset__18_, r_2_
	ori	$3, $0, 7
# was:	ori	_assign__16_, 0, 7
	sw	$3, 0($2)
# was:	sw	_assign__16_, 0(_add__19_)
# 	ori	0,_assign__16_,0
	ori	$2, $0, 3
# was:	ori	_exp__22_, 0, 3
	sll	$2, $2, 2
# was:	sll	_offset__23_, _exp__22_, 2
	add	$2, $2, $16
# was:	add	_add__24_, _offset__23_, r_2_
	ori	$3, $0, 4
# was:	ori	_assign__21_, 0, 4
	sw	$3, 0($2)
# was:	sw	_assign__21_, 0(_add__24_)
# 	ori	0,_assign__21_,0
	lw	$2, 0($16)
# was:	lw	_plus1__27_, 0(r_2_)
	ori	$3, $0, 1
# was:	ori	_plus2__28_, 0, 1
	add	$2, $2, $3
# was:	add	_assign__26_, _plus1__27_, _plus2__28_
	sw	$2, 0($16)
# was:	sw	_assign__26_, 0(a_1_)
# 	ori	0,_assign__26_,0
	lw	$2, 0($16)
# was:	lw	_exps__29_, 0(a_1_)
# 	ori	2,_exps__29_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	0,2,0
	ori	$2, $0, 0
# was:	ori	_return__30_, 0, 0
# 	ori	2,_return__30_,0
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
