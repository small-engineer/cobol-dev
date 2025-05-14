#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"
#include "http_log.h"
#include "ap_config.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <utf8proc.h>

#define COBOL_SOCK_PATH "/var/run/cobol/cobol.sock"
#define BUFSIZE 4096

/* UTF-8 エスケープ */
static void
json_escape_utf8(char *out, const char *in, size_t max)
{
    size_t j = 0;
    const uint8_t *p = (const uint8_t *)in;

    while (*p && j + 1 < max) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t len = utf8proc_iterate(p, -1, &cp);
        if (len <= 0) break;
        p += len;

        if (j + 6 >= max) break;

        switch (cp) {
        case '\"': out[j++] = '\\'; out[j++] = '\"'; break;
        case '\\': out[j++] = '\\'; out[j++] = '\\'; break;
        case '\n': out[j++] = '\\'; out[j++] = 'n';  break;
        case '\r': out[j++] = '\\'; out[j++] = 'r';  break;
        case '\t': out[j++] = '\\'; out[j++] = 't';  break;
        default:
            if (cp < 0x20 || cp == 0x7f) {
                j += snprintf(out + j, max - j, "\\u%04x", cp);
            } else {
                j += utf8proc_encode_char(cp, (uint8_t *)(out + j));
            }
        }
    }
    out[j] = '\0';
}

static int
cobol_handler(request_rec *r)
{
    if (!r->handler || strcmp(r->handler, "cobol-server") != 0) {
        return DECLINED;
    }

    /* JSON で返却 */
    r->content_type = "application/json";

    /* ヘッダーオンリーは OK */
    if (r->header_only) {
        return OK;
    }

    /* POST のみ受付 */
    if (strcmp(r->method, "POST") != 0) {
        return HTTP_METHOD_NOT_ALLOWED;
    }

    /* クライアントボディ準備 */
    if (ap_setup_client_block(r, REQUEST_CHUNKED_ERROR) != OK) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, "ap_setup_client_block() failed");
        return HTTP_INTERNAL_SERVER_ERROR;
    }

    if (!ap_should_client_block(r)) {
        ap_rputs("{\"status\":\"error\",\"msg\":\"no request body\"}\n", r);
        return OK;
    }

    /* POST ボディ読み取り */
    char input[BUFSIZE];
    memset(input, 0, sizeof(input));
    int total_read = 0;
    int bytes_read;
    while ((bytes_read = ap_get_client_block(r, input + total_read,
                    sizeof(input) - total_read - 1)) > 0) {
        total_read += bytes_read;
        if (total_read >= (int)(sizeof(input) - 1)) {
            break;
        }
    }
    input[total_read] = '\0';

    /* ソケット接続 */
    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sock < 0) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "socket() failed");
        ap_rputs("{\"status\":\"error\",\"msg\":\"socket error\"}\n", r);
        return OK;
    }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, COBOL_SOCK_PATH, sizeof(addr.sun_path) - 1);

    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r,
                      "connect(%s) failed", COBOL_SOCK_PATH);
        close(sock);
        ap_rputs("{\"status\":\"error\",\"msg\":\"connect failed\"}\n", r);
        return OK;
    }

    /* COBOL 側へ送信（改行付き） */
    if (write(sock, input, total_read) < 0 ||
        write(sock, "\n", 1) != 1) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "write() to COBOL failed");
        close(sock);
        ap_rputs("{\"status\":\"error\",\"msg\":\"write failed\"}\n", r);
        return OK;
    }

    /* COBOL からのレスポンス受信 */
    char buf[BUFSIZE];
    memset(buf, 0, sizeof(buf));
    ssize_t len = read(sock, buf, sizeof(buf) - 1);
    close(sock);

    if (len > 0) {
        buf[len] = '\0';
        char escaped[BUFSIZE * 2];
        memset(escaped, 0, sizeof(escaped));
        json_escape_utf8(escaped, buf, sizeof(escaped));
        ap_rprintf(r, "{\"status\":\"ok\",\"cobol\":\"%s\"}\n", escaped);
    } else {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "read() failed or zero bytes");
        ap_rputs("{\"status\":\"error\",\"msg\":\"read error\"}\n", r);
    }

    return OK;
}

static void
cobol_register_hooks(apr_pool_t *p)
{
    ap_hook_handler(cobol_handler, NULL, NULL, APR_HOOK_MIDDLE);
}

module AP_MODULE_DECLARE_DATA cobol_server_module = {
    STANDARD20_MODULE_STUFF,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    cobol_register_hooks
};
