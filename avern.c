#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    union {
        void *ptr;
        int i;
    };
} constant;

typedef struct {
    unsigned* i;
    constant *c;
    char *name;
} fun;

fun *funs;
int fun_index = 0;

void install_fun(char *name, unsigned instructions[], int ic, constant *c)
{
    fun *f = malloc(sizeof(fun));
    f->i = malloc(ic * sizeof(unsigned));
    for (int i = 0; i < ic; i++) {
        f->i[i] = instructions[i];
    }
    f->name = malloc(strlen(name));
    f->c = c;
    strcpy(f->name, name);
    funs[fun_index] = *f;
    fun_index++;
}

fun *lookup_fun(char *name)
{
    for (int i = 0; i < fun_index; i++) {
        if (strcmp(funs[i].name, name) == 0) {
            return &funs[i];
        }
    }

    return NULL;
}

void install_main1()
{
    // Simple addition
    unsigned instructions[4] = {0x1064, 0x11C8, 0x2201, 0x0000};
    install_fun("main", instructions, 4, NULL);
}

void install_main()
{
    // Simple addition with a function call
    constant* c = malloc(sizeof(constant));
    c->ptr = (void*) lookup_fun("double");
    unsigned instructions[6] = {0x6000, 0x1164, 0x3010, 0x1064, 0x2210, 0x0000};
    install_fun("main", instructions, 6, c);
}

void install_double()
{
    unsigned instructions[4] = {0x5100, 0x2210, 0x5020, 0x4000};
    install_fun("double", instructions, 4, NULL);
}

int main()
{

    funs = malloc(100 * sizeof(fun*));
    // Install some functions!
    install_double();
    install_main();
    constant *stack = malloc(10 * sizeof(constant));
    fun* f = lookup_fun("main");
    int pc = 0;
    int sb = 0;
    // These vars save fun/program counter/stack base arguments for calls
    fun* sf;
    int spc, ssb;
    int running = 1;
    int instruction, op, ra, rb, rc, imm, c;
    while (running) {
        int instruction = f->i[pc++];
        op = (instruction & 0xF000) >> 12;
        printf("inst is %d, op is %d\r\n", instruction, op);
        switch (op) {
        case 0: // HALT
            running = 0;
            break;
        case 1: // MOVE IMMEDIATE
            ra = (instruction & 0xF00) >> 8;
            imm = (instruction & 0xFF);
            stack[sb+ra].i = imm;
            break;
        case 2: // ADD
            ra = (instruction & 0xF00) >> 8;
            rb = (instruction & 0xF0) >> 4;
            rc = (instruction & 0xF);
            stack[sb+ra].i = stack[sb+rb].i + stack[sb+rc].i;
            printf("stack[sb+ra].i is %d\r\n", stack[sb+ra].i);
            break;
        case 3: // CALL
            ra = (instruction & 0xF00) >> 8;
            rb = (instruction & 0xF0) >> 4;
            spc = pc;
            ssb = sb;
            sf = f;
            f = stack[sb+ra].ptr;
            pc = 0;
            sb = sb+rb;
            break;
        case 4: // RETURN
            f = sf;
            pc = spc;
            sb = ssb;
            break;
        case 5: // MOVE
            ra = (instruction & 0xF00) >> 8;
            rb = (instruction & 0xF0) >> 4;
            stack[sb+ra] = stack[sb+rb];
            break;
        case 6: // LOADK
            ra = (instruction & 0xF00) >> 8;
            c = (instruction & 0xFF);
            stack[sb+ra] = f->c[c];
            break;
        }
    }
    return 0;
}
