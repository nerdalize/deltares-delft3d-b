#include "meminfo.h"

#ifdef WIN32

unsigned __int64 MemInfo::GetTotalMemSize()
{
	MEMORYSTATUSEX status;
    status.dwLength = sizeof(status);
    GlobalMemoryStatusEx(&status);
 	return unsigned __int64 (status.ullTotalPhys);
}

#else

unsigned long long MemInfo::GetTotalMemSize()
{
	long long pages = sysconf(_SC_PHYS_PAGES);
    long long page_size = sysconf(_SC_PAGE_SIZE);
    return (pages * page_size);
}

#endif
