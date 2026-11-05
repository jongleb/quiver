# A TLS front for the fake HTTP/2 server of test_http2: one connection, its bytes passed on.
# Arguments: the backend's port, the certificate and key it presents, the ALPN protocol it
# accepts ("-" for none), the CAs a client certificate must chain to ("-" when none is asked).
import socket
import ssl
import sys
import threading

backend, certificate, key, alpn, clients = sys.argv[1:6]
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(certificate, key)
if alpn != "-":
    context.set_alpn_protocols([alpn])
if clients != "-":
    context.verify_mode = ssl.CERT_REQUIRED
    context.load_verify_locations(clients)
listener = socket.socket()
listener.bind(("127.0.0.1", 0))
listener.listen(1)
print(listener.getsockname()[1], flush=True)
listener.settimeout(10)
connection, _ = listener.accept()
upstream = socket.create_connection(("127.0.0.1", int(backend)))
try:
    tls = context.wrap_socket(connection, server_side=True)
except (ssl.SSLError, OSError):
    upstream.close()
    sys.exit(0)


def pipe(source, sink):
    try:
        while True:
            data = source.recv(65536)
            if not data:
                break
            sink.sendall(data)
    except OSError:
        pass
    try:
        sink.shutdown(socket.SHUT_WR)
    except OSError:
        pass


back = threading.Thread(target=pipe, args=(upstream, tls))
back.start()
pipe(tls, upstream)
back.join()
