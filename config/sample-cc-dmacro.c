#include <stdio.h>
#define _STR2_(x) #x
#define _STR1_(x) _STR2_(x)
#define _POUT_(x) "#define " #x " " _STR1_(x) "\n"
int main(void) {
#if defined(__STDC__)
   printf(_POUT_(__STDC__));
#endif
#if defined(__STDC_HOSTED__)
   printf(_POUT_(__STDC_HOSTED__));
#endif
#if defined(__STDC_NO_ATOMICS__)
   printf(_POUT_(__STDC_NO_ATOMICS__));
#endif
#if defined(__STDC_NO_COMPLEX__)
   printf(_POUT_(__STDC_NO_COMPLEX__));
#endif
#if defined(__STDC_NO_THREADS__)
   printf(_POUT_(__STDC_NO_THREADS__));
#endif
#if defined(__STDC_NO_VLA__)
   printf(_POUT_(__STDC_NO_VLA__));
#endif
#if defined(__STDC_VERSION__)
   printf(_POUT_(__STDC_VERSION__));
#endif
#if defined(_DEBUG)
  printf(_POUT_(_DEBUG));
#endif
#if defined(_DLL)
  printf(_POUT_(_DLL));
#endif
#if defined(_WIN32)
  printf(_POUT_(_WIN32));
#endif
#if defined(_WIN64)
   printf(_POUT_(_WIN64));
#endif
#if defined(_WINRT_DLL)
  printf(_POUT_(_WINRT_DLL));
#endif
#if defined(_MSC_BUILD)
  printf(_POUT_(_MSC_BUILD));
#endif
#if defined(_MSC_EXTENSIONS)
  printf(_POUT_(_MSC_EXTENSIONS));
#endif
#if defined(_MSC_FULL_VER)
  printf(_POUT_(_MSC_FULL_VER));
#endif
#if defined(_MSC_VER)
  printf(_POUT_(_MSC_VER));
#endif
#if defined(__MSVC_RUNTIME_CHECKS)
  printf(_POUT_(__MSVC_RUNTIME_CHECKS));
#endif
#if defined(_MT)
  printf(_POUT_(_MT));
#endif
#if defined(_OPENMP)
  printf(_POUT_(_OPENMP));
#endif
#if defined(__SANITIZE_ADDRESS__)
  printf(_POUT_(__SANITIZE_ADDRESS__));
#endif
#if defined(_WCHAR_T_DEFINED)
  printf(_POUT_(_WCHAR_T_DEFINED));
#endif
}
