$NetBSD: patch-src_unix_netbsd.c,v 1.1 2017/10/06 15:58:15 kamil Exp $

--- a/src/unix/netbsd.c
+++ b/src/unix/netbsd.c
@@ -73,6 +73,7 @@ void uv_loadavg(double avg[3]) {
 
 
 int uv_exepath(char* buffer, size_t* size) {
+#if defined(KERN_PROC_PATHNAME)
   /* Intermediate buffer, retrieving partial path name does not work
    * As of NetBSD-8(beta), vnode->path translator does not handle files
    * with longer names than 31 characters.
@@ -100,6 +101,22 @@ int uv_exepath(char* buffer, size_t* size) {
 
   /* Set new size. */
   *size = strlen(buffer);
+#else
+  ssize_t n;
+
+  if (buffer == NULL || size == NULL || *size == 0)
+    return -EINVAL;
+
+  n = *size - 1;
+  if (n > 0)
+    n = readlink("/proc/self/exe", buffer, n);
+
+  if (n == -1)
+    return -errno;
+
+  buffer[n] = '\0';
+  *size = n;
+#endif
 
   return 0;
 }
