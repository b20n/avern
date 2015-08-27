#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

const size_t SIZE = 1024;
typedef long (*JittedFunc)(long);

void* alloc_writable_memory(size_t size) {
    void* ptr = mmap(
        0,
        size,
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANON, // MAP_ANONYMOUS when not on OSX
        -1,
        0
    );
    if (ptr == (void*)-1) {
        return NULL;
    }
    return ptr;
}

int make_memory_executable(void* m, size_t size) {
    if (mprotect(m, size, PROT_READ | PROT_EXEC) == -1) {
        return -1;
    }
    return 0;
}

void emit_code_into_memory(unsigned char* m) {
    unsigned char code[] = {
        0x48, 0x89, 0xf8,                   // mov %rdi, %rax
        0x48, 0x83, 0xc0, 0x04,             // add $4, %rax
        0xc3                                // ret
    };
    memcpy(m, code, sizeof(code));
}

void emit_to_rw_run_from_rx() {
    void* m = alloc_writable_memory(SIZE);
    emit_code_into_memory(m);
    make_memory_executable(m, SIZE);

    JittedFunc func = m;
    int result = func(2);
    printf("result = %d\n", result);
}

int main() {
    emit_to_rw_run_from_rx();
}
