
!#define TIMER_CPU_TIME
#define TIMER_F90_EPTIME

!// if not using openmp, can't use the openmp timer
#if !defined(_OPENMP) && defined(TIMER_OMP_GET_WTIME)
#  undef TIMER_OMP_GET_WTIME
!#define TIMER_CPU_TIME
#  define TIMER_F90_EPTIME
#endif

#if defined(TIMER_F90_EPTIME)
#  define TSTART(x) x=epelapsedtime()
#  define TSTOP(x)  x=epelapsedtime()
#  define TSTAMP(x) x=epelapsedtime()
#elif defined(TIMER_CPU_TIME)
#  define TSTART(x) call CPU_TIME(x)
#  define TSTOP(x)  call CPU_TIME(x)
#  define TSTAMP(x) call CPU_TIME(x)
#elif defined(TIMER_OMP_GET_WTIME)
#  define TSTART(x) x=OMP_get_wtime()
#  define TSTOP(x)  x=OMP_get_wtime()
#  define TSTAMP(x) x=OMP_get_wtime()
#else
  NEED_TO_SPECIFY_TIMER
#endif

!#define EXPAND_POW4
#ifdef EXPAND_POW4
#define POW4(a) (a)*(a)*(a)*(a)
#else
#define POW4(a) (a)**4
#endif

#define PRINT_TIME0(a, t) write(*,'(a80,f16.4)') a, t
#define PRINT_TIME1(a, t) write(*,'(a70,f16.4)') a, t
#define PRINT_TIME2(a, t) write(*,'(a60,f10.4)') a, t
#define PRINT_TIME2i(a, d, t) write(*,'(a56,i4,f10.4)') a,d,t
#define PRINT_TIME3(a, t) write(*,'(a50,f10.4)') a, t
#define PRINT_TIME3i(a, d, t) write(*,'(a46,i4,f10.4)') a,d,t
#define PRINT_TIME4(a, t) write(*,'(a40,f10.4)') a, t

#define PRINT_TIMEX(a, t) write(*,'(a100,f16.6)') a, t

#define PRINTES(a, t) write(*,'(a80,es22.15)') a, t

#define PRINT_TIME_AF(a,     t) write(*,'(a55,10x,f16.4)') a,    t
#define PRINT_TIME_AIF(a, i, t) write(*,'(a55,i10,f16.4)') a, i, t
#define PRINT_TIME_AIIF(a, i1, i2, t) write(*,'(a55,i10,i10,f16.4)') a, i1, i2, t
#define PRINT_TIME_AIIIF(a, i1, i2, i3, t) write(*,'(a55,i10,i10,i10,f16.55)') a, i1, i2, i3, t

!// integer :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#ifdef _OPENMP
#define THREADID(a)  OMP_GET_THREAD_NUM()
#define NUMTHREADS(a) OMP_GET_NUM_THREADS()
#define MAXTHREADS(a) OMP_GET_MAX_THREADS()
#else
#define THREADID(a)  1
#define NUMTHREADS(a) 1
#define MAXTHREADS(a) 1
#endif
