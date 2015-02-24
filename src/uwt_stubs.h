#define P1(x)                                   \
  CAMLextern value x(value)

#define P2(x)                                   \
  CAMLextern value x(value,value)

#define P3(x)                                   \
  CAMLextern value x(value,value,value)

#define P4(x)                                   \
  CAMLextern value x(value,value,value,value)

#define P5(x)                                       \
  CAMLextern value x(value,value,value,value,value)

#define P6(x)                                             \
  CAMLextern value x(value,value,value,value,value,value)

#define P7(x)                                                   \
  CAMLextern value x(value,value,value,value,value,value,value)

#define BY(x)                                   \
  CAMLextern value x(value*,int)

P1(uwt_init_stacks_na);

P1(uwt_strerror);

P1(uwt_default_loop);
P2(uwt_run_loop);
P1(uwt_loop_close_na);

P2(uwt_req_create);
P1(uwt_req_cancel_noerr_na);

P6(uwt_fs_open_native);
BY(uwt_fs_open_byte);
P7(uwt_fs_read_native);
BY(uwt_fs_read_byte);
P7(uwt_fs_write_native);
BY(uwt_fs_write_byte);
P4(uwt_fs_close);
P4(uwt_fs_unlink);
P5(uwt_fs_mkdir);
P4(uwt_fs_rmdir);
P5(uwt_fs_rename);
P5(uwt_fs_link);
P4(uwt_fs_fsync);
P4(uwt_fs_fdatasync);
P5(uwt_fs_ftruncate);
P7(uwt_fs_sendfile_native);
BY(uwt_fs_sendfile_byte);
P4(uwt_fs_scandir);
P4(uwt_fs_mkdtemp);
P4(uwt_fs_readlink);
P5(uwt_fs_access);
P5(uwt_fs_chmod);
P5(uwt_fs_fchmod);
P6(uwt_fs_chown_native);
BY(uwt_fs_chown_byte);
P6(uwt_fs_fchown_native);
BY(uwt_fs_fchown_byte);
P6(uwt_fs_utime_native);
BY(uwt_fs_utime_byte);
P6(uwt_fs_futime_native);
BY(uwt_fs_futime_byte);
P6(uwt_fs_symlink_native);
BY(uwt_fs_symlink_byte);
P4(uwt_fs_stat);
P4(uwt_fs_lstat);
P4(uwt_fs_fstat);

P2(uwt_close);
P2(uwt_get_buffer_size_common_na);
P3(uwt_set_buffer_size_common_na);
P1(uwt_close_noerr);
P1(uwt_is_active_na);

P1(uwt_is_readable_na);
P1(uwt_is_writable_na);
P1(uwt_write_queue_size_na);
P2(uwt_shutdown);
P3(uwt_listen);
P1(uwt_accept);
P2(uwt_accept_raw_na);
P2(uwt_read_start);
P1(uwt_read_stop);
P5(uwt_read_own);
P6(uwt_udp_send_native);
BY(uwt_udp_send_byte);
P5(uwt_write);
P6(uwt_write2_native);
BY(uwt_write2_byte);
P5(uwt_udp_try_send);
P4(uwt_try_write);

P3(uwt_tty_init);
P2(uwt_tty_set_mode_na);
P1(uwt_tty_reset_mode_na);
P1(uwt_tty_get_winsize);

P3(uwt_pipe_open);
P2(uwt_pipe_init);
P2(uwt_pipe_bind_na);
P1(uwt_pipe_getsockname);
P2(uwt_pipe_pending_instances_na);
P1(uwt_pipe_pending_count_na);
P1(uwt_pipe_pending_type_na);
P3(uwt_pipe_connect);

P1(uwt_tcp_init);
P1(uwt_udp_init);
P2(uwt_tcp_open_na);
P2(uwt_udp_open_na);
P3(uwt_tcp_bind_na);
P3(uwt_udp_bind_na);
P2(uwt_tcp_nodelay_na);
P3(uwt_tcp_keepalive_na);
P2(uwt_tcp_simultaneous_accepts_na);
P1(uwt_tcp_getsockname);
P1(uwt_tcp_getpeername);
P1(uwt_udp_getsockname);
P3(uwt_tcp_connect);

P4(uwt_udp_set_membership_na);
P2(uwt_udp_set_multicast_loop_na);
P2(uwt_udp_set_multicast_ttl_na);
P2(uwt_udp_set_multicast_interface_na);
P2(uwt_udp_set_broadcast_na);
P2(uwt_udp_set_ttl_na);
P2(uwt_udp_recv_start);
P1(uwt_udp_recv_stop);
P1(uwt_udp_send_queue_size_na);
P1(uwt_udp_send_queue_count_na);

P4(uwt_timer_start);
P1(uwt_timer_stop);

P3(uwt_signal_start);
P1(uwt_signal_stop);

P4(uwt_poll_start);
P4(uwt_poll_start_socket);
P1(uwt_poll_stop);

P4(uwt_fs_event_start);
P1(uwt_fs_event_stop);

P4(uwt_fs_poll_start);
P1(uwt_fs_poll_stop);

P1(uwt_version_na);
P1(uwt_version_string);
P1(uwt_resident_set_memory);
P1(uwt_uptime);
P2(uwt_ip4_addr);
P1(uwt_ip4_name);
P2(uwt_ip6_addr);
P1(uwt_ip6_name);
P1(uwt_cpu_info);
P1(uwt_interface_addresses);
P1(uwt_load_avg);
P1(uwt_get_total_memory);
P1(uwt_hrtime);
P1(uwt_getrusage);

P6(uwt_getaddrinfo_native);
BY(uwt_getaddrinfo_byte);
P5(uwt_getnameinfo);

P1(uwt_disable_stdio_inheritance_na);
P4(uwt_spawn);
P1(uwt_pid_na);
P2(uwt_process_kill_na);
P2(uwt_kill_na);

P1(uwt_get_fd);
P1(uwt_get_socket);
P1(uwt_to_sockaddr);
P1(uwt_of_sockaddr);
P1(uwt_sun_path);

P5(uwt_getservbyname);
P5(uwt_getservbyport);
P4(uwt_gethostbyname);
P4(uwt_gethostbyaddr);
P3(uwt_gethostname);
P4(uwt_getprotobyname);
P4(uwt_getprotobynumber);
P6(uwt_lseek_native);
BY(uwt_lseek_byte);

/* valgrind */
P1(uwt_free_all_memory);

#undef P1
#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef BY
