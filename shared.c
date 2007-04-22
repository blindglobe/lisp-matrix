#include <dlfcn.h>
#include <stdlib.h> /* #defines NULL */

void*
open_library (char* library_name)
{
  return dlopen (library_name, RTLD_LAZY);
}

/**
 * Returns zero on success, nonzero on error.
 */
int
close_library (void* lib)
{
  return dlclose (lib);
}

int
probe_library (char* function_name, char* library_name)
{
  void* lib = open_library (library_name);
  if (lib == NULL)
    return 0;
  else
    {
      void* fun = dlsym (lib, function_name);
      close_library (lib);
      return fun != NULL;
    }
}
