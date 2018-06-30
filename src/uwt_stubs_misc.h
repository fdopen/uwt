/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_MISC_H
#define __UWT_STUBS_MISC_H

#include "uwt_stubs_base.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_guess_handle_na);
UWT_EXTERN1(uwt_version_na);
UWT_EXTERN1(uwt_version_string);
UWT_EXTERN1(uwt_resident_set_memory);
UWT_EXTERN1(uwt_uptime);
UWT_EXTERN2(uwt_ip4_addr);
UWT_EXTERN1(uwt_ip4_name);
UWT_EXTERN2(uwt_ip6_addr);
UWT_EXTERN1(uwt_ip6_name);
UWT_EXTERN1(uwt_cpu_info);
UWT_EXTERN1(uwt_interface_addresses);
UWT_EXTERN1(uwt_load_avg);
UWT_EXTERN1(uwt_get_total_memory);
UWT_EXTERN1(uwt_hrtime);
UWT_EXTERN1(uwt_getrusage);
UWT_EXTERN1(uwt_os_homedir);
UWT_EXTERN1(uwt_os_tmpdir);
UWT_EXTERN1(uwt_get_passwd);
UWT_EXTERN1(uwt_exepath);
UWT_EXTERN1(uwt_cwd);
UWT_EXTERN1(uwt_get_process_title);
UWT_EXTERN2(uwt_set_process_title_na);
UWT_EXTERN2(uwt_print_all_handles);
UWT_EXTERN2(uwt_print_active_handles);
UWT_EXTERN1(uwt_chdir);
UWT_EXTERN1(uwt_os_getenv);
UWT_EXTERN2(uwt_os_setenv_na);
UWT_EXTERN1(uwt_os_unsetenv_na);
UWT_EXTERN1(uwt_os_getppid_na);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_MISC_H */
