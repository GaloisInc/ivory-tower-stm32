
void ivory_user_assert_hook(void) {
	asm volatile("bkpt");
}
