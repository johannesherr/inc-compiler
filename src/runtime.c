#include <stdio.h>
#include <sys/mman.h>

#define bool_f 0x2F
#define bool_t 0x6F
#define fx_mask 0x03
#define fx_tag 0x00
#define fx_shift 2
#define ch_mask 0xFF
#define ch_tag 0x0F
#define ch_shift 8
#define nil 0x3F
#define obj_mask 0x07
#define string_tag 0x06

typedef unsigned int ptr;

static void print_string(ptr x) {
  int *p = (int*) (x - string_tag);
  int length = *p;
  int i;
  length = length >> fx_shift;
  char *cs = (char*) (p + 1);
  printf("\"");
  for (i = 0; i < length; i++) {
    printf("%c", *(cs + i));
  }
  printf("\"");
}

static void print_ptr(ptr x) {
  if ((x & fx_mask) == fx_tag) {
    printf("%d", ((int) x) >> fx_shift);
  } else if ((x & ch_mask) == ch_tag) {
    printf("#\\%c", ((int) x) >> ch_shift);
  } else if ((x & obj_mask) == string_tag) {
    print_string(x);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == nil) {
    printf("()");
  } else {
    printf("unkown: 0x%08x", x);
  }
  printf("\n");
}

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED) { printf("map failed"); }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) { printf("protect failed"); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) { printf("protect failed"); }
  return p + page;
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) { printf("unmap failed"); }
}

typedef struct {
  void* eax;
  void* ebx;
  void* ecx;
  void* edx;
  void* esi;
  void* edi;
  void* ebp;
  void* esp;
} context;

int main(int argc, char** argv) {
  int stack_size = 16 * 4096;
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;

  int heap_size = 16 * 4096;
  char* heap_top = allocate_protected_space(heap_size);

  context ctx;

  print_ptr(scheme_entry(&ctx, stack_base, heap_top));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
