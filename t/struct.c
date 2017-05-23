/* Or how to figure out if you messed up CFFI. */

#include <stddef.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

int main(int arg, char* argv[])
{
  struct stat foo;
  printf("stat sizeof: %zu\n", sizeof(struct stat));
  printf("st_mode sizeof: %zu\n", sizeof(foo.st_mode));
  printf("st_mode offset: %zu\n", offsetof(struct stat, st_mode));
  printf("st_ino sizeof: %zu\n", sizeof(foo.st_ino));
  printf("st_ino offset: %zu\n", offsetof(struct stat, st_ino));
  printf("st_dev sizeof: %zu\n", sizeof(foo.st_dev));
  printf("st_dev offset: %zu\n", offsetof(struct stat, st_dev));
  printf("st_nlink sizeof: %zu\n", sizeof(foo.st_nlink));
  printf("st_nlink offset: %zu\n", offsetof(struct stat, st_nlink));
  printf("st_uid sizeof: %zu\n", sizeof(foo.st_uid));
  printf("st_uid offset: %zu\n", offsetof(struct stat, st_uid));
  printf("st_gid sizeof: %zu\n", sizeof(foo.st_gid));
  printf("st_gid offset: %zu\n", offsetof(struct stat, st_gid));
  printf("st_size sizeof: %zu\n", sizeof(foo.st_size));
  printf("st_size offset: %zu\n", offsetof(struct stat, st_size));
  printf("st_atime sizeof: %zu\n", sizeof(foo.st_atime));
  printf("st_atime offset: %zu\n", offsetof(struct stat, st_atime));
  printf("st_mtime sizeof: %zu\n", sizeof(foo.st_mtime));
  printf("st_mtime offset: %zu\n", offsetof(struct stat, st_mtime));
  printf("st_ctime sizeof: %zu\n", sizeof(foo.st_ctime));
  printf("st_ctime offset: %zu\n", offsetof(struct stat, st_ctime));
}
