#!/usr/bin/env python3
import base64
import hashlib
import select
import socket
import subprocess
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import parse_qs, urlparse
from urllib.request import Request, urlopen

SHELL_PORT = 8765
PREVIEW_PORT = 23635
WS_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

HTML_SHELL = r"""<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Zettel Preview Shell</title>
    <style>
      body {
        margin: 0;
        display: flex;
        flex-direction: column;
        height: 100vh;
        font-family: sans-serif;
      }
      .bar {
        padding: 8px 12px;
        background: #1e1e1e;
        color: #eee;
        display: flex;
        gap: 12px;
        align-items: center;
      }
      .status {
        font-size: 0.85rem;
        color: #aaa;
      }
      iframe {
        flex: 1;
        border: 0;
        width: 100%;
      }
    </style>
  </head>
  <body>
    <div class="bar">
      <span>Zettel Preview Shell</span>
      <button type="button" onclick="reloadFrame()">Reload Preview</button>
      <span id="status" class="status">Ready</span>
    </div>
    <iframe id="preview" src="/"></iframe>
    <script>
      const iframe = document.getElementById("preview");
      const statusEl = document.getElementById("status");

      function setStatus(text) {
        statusEl.textContent = text;
      }

      function reloadFrame() {
        iframe.src = "/?t=" + Date.now();
      }

      function getLinkHref(anchor) {
        return (
          anchor.getAttribute("href") ||
          anchor.getAttributeNS("http://www.w3.org/1999/xlink", "href") ||
          anchor.getAttribute("xlink:href")
        );
      }

      function bindIframeInterceptor() {
        let doc;
        try {
          doc = iframe.contentDocument;
        } catch {
          return;
        }
        if (!doc || doc.__zettelBound) return;
        doc.__zettelBound = true;

        doc.addEventListener(
          "click",
          (event) => {
            const targetEl =
              event.target && typeof event.target.closest === "function"
                ? event.target
                : event.target && event.target.parentElement;
            const anchor = targetEl ? targetEl.closest("a") : null;
            if (!anchor) return;

            const rawHref = getLinkHref(anchor);
            if (!rawHref) return;

            let parsed;
            try {
              parsed = new URL(rawHref, doc.baseURI || window.location.href);
            } catch {
              return;
            }

            if (parsed.protocol === "typst-note:") {
              event.preventDefault();
              event.stopPropagation();
              anchor.removeAttribute("target");

              const noteId = decodeURIComponent(
                parsed.hostname || parsed.pathname.replace(/^\/+/, "")
              );
              if (!noteId) return;

              setStatus("Switching note...");
              fetch("/open?note=" + encodeURIComponent(noteId), { method: "GET" })
                .then((resp) => {
                  if (!resp.ok) {
                    throw new Error("HTTP " + resp.status);
                  }
                  setStatus("Loaded");
                  reloadFrame();
                })
                .catch((err) => {
                  setStatus("Error: " + err.message);
                });
            }
          },
          true
        );
      }

      iframe.addEventListener("load", bindIframeInterceptor);
      setInterval(bindIframeInterceptor, 500);
    </script>
  </body>
</html>
"""


class Previewer:
    def __init__(self, root: Path, port: int = PREVIEW_PORT):
        self.root = root.resolve()
        self.port = port
        self.proc: subprocess.Popen | None = None

    def switch_file(self, file: Path) -> None:
        resolved = file.resolve()
        if not resolved.exists():
            raise FileNotFoundError(f"Missing note: {resolved}")

        if self.proc and self.proc.poll() is None:
            self.proc.terminate()
            self.proc.wait()

        cmd = [
            "tinymist",
            "preview",
            str(resolved),
            "--root",
            str(self.root),
            "--input",
            "preview=1",
            f"--data-plane-host=127.0.0.1:{self.port}",
            "--no-open",
        ]
        self.proc = subprocess.Popen(cmd)

    def stop(self) -> None:
        if self.proc and self.proc.poll() is None:
            self.proc.terminate()
            self.proc.wait()


def wait_for_port(host: str, port: int, timeout: float = 4.0) -> bool:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            with socket.create_connection((host, port), timeout=0.1):
                return True
        except OSError:
            time.sleep(0.05)
    return False


def resolve_note(root: Path, note_id: str) -> Path:
    candidate = root / f"{note_id}.typ"
    if candidate.exists():
        return candidate

    matches = list(root.rglob(f"*{note_id}*.typ"))
    if not matches:
        raise FileNotFoundError(f"No note matching {note_id!r}")
    return matches[0]


def _relay_sockets(a: socket.socket, b: socket.socket) -> None:
    try:
        while True:
            readable, _, exceptional = select.select([a, b], [], [a, b], 1.0)
            if exceptional:
                break
            for sock in readable:
                other = b if sock is a else a
                data = sock.recv(65536)
                if not data:
                    return
                other.sendall(data)
    except OSError:
        pass
    finally:
        for s in (a, b):
            try:
                s.close()
            except OSError:
                pass


def make_handler(previewer: Previewer, shell_html: str):
    html_bytes = shell_html.encode("utf-8")

    class Handler(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def _send_bytes(self, status: int, content_type: str, body: bytes) -> None:
            self.send_response(status)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def _handle_websocket_proxy(self, upstream_path: str) -> None:
            ws_key = self.headers.get("Sec-WebSocket-Key")
            if not ws_key:
                self._send_bytes(400, "text/plain; charset=utf-8", b"Missing Sec-WebSocket-Key")
                return

            try:
                upstream = socket.create_connection(("127.0.0.1", previewer.port), timeout=3.0)
            except OSError as exc:
                self._send_bytes(502, "text/plain; charset=utf-8", str(exc).encode("utf-8"))
                return

            req_lines = [f"GET {upstream_path} HTTP/1.1", f"Host: 127.0.0.1:{previewer.port}"]
            for k, v in self.headers.items():
                if k.lower() == "host":
                    continue
                req_lines.append(f"{k}: {v}")
            req_lines.append("\r\n")
            upstream.sendall("\r\n".join(req_lines).encode("latin-1"))

            buf = b""
            while b"\r\n\r\n" not in buf:
                chunk = upstream.recv(4096)
                if not chunk:
                    upstream.close()
                    self._send_bytes(502, "text/plain; charset=utf-8", b"Upstream WS handshake failed")
                    return
                buf += chunk

            header_part, remainder = buf.split(b"\r\n\r\n", 1)
            status_line = header_part.split(b"\r\n", 1)[0]
            if b" 101 " not in status_line:
                upstream.close()
                self._send_bytes(502, "text/plain; charset=utf-8", status_line)
                return

            accept_digest = hashlib.sha1((ws_key + WS_GUID).encode("ascii")).digest()
            accept_val = base64.b64encode(accept_digest).decode("ascii")

            self.send_response(101, "Switching Protocols")
            self.send_header("Upgrade", "websocket")
            self.send_header("Connection", "Upgrade")
            self.send_header("Sec-WebSocket-Accept", accept_val)
            self.end_headers()

            client_sock = self.connection
            client_sock.setblocking(False)
            upstream.setblocking(False)
            if remainder:
                try:
                    client_sock.sendall(remainder)
                except OSError:
                    pass

            _relay_sockets(client_sock, upstream)
            self.close_connection = True

        def _proxy_http(self, upstream_path: str) -> None:
            target_url = f"http://127.0.0.1:{previewer.port}{upstream_path}"
            req = Request(target_url, method=self.command)
            for k, v in self.headers.items():
                if k.lower() in {"host", "connection"}:
                    continue
                req.add_header(k, v)

            try:
                with urlopen(req, timeout=5.0) as resp:
                    body = resp.read()
                    content_type = resp.headers.get("Content-Type", "application/octet-stream")
                    self.send_response(resp.status)
                    for k, v in resp.headers.items():
                        if k.lower() in {"transfer-encoding", "connection", "content-length"}:
                            continue
                        self.send_header(k, v)
                    self.send_header("Content-Type", content_type)
                    self.send_header("Content-Length", str(len(body)))
                    self.end_headers()
                    self.wfile.write(body)
            except HTTPError as err:
                body = err.read()
                self._send_bytes(
                    err.code,
                    err.headers.get("Content-Type", "text/plain; charset=utf-8"),
                    body,
                )
            except URLError as exc:
                self._send_bytes(
                    502,
                    "text/plain; charset=utf-8",
                    f"Preview unavailable: {exc}".encode(),
                )

        def do_GET(self) -> None:
            parsed = urlparse(self.path)

            if parsed.path == "/shell":
                self._send_bytes(200, "text/html; charset=utf-8", html_bytes)
                return

            if parsed.path == "/open":
                note = parse_qs(parsed.query).get("note", [None])[0]
                if not note:
                    self._send_bytes(400, "text/plain; charset=utf-8", b"Missing 'note' query parameter")
                    return

                try:
                    target = resolve_note(previewer.root, note)
                    previewer.switch_file(target)
                    wait_for_port("127.0.0.1", previewer.port)
                    self._send_bytes(200, "application/json; charset=utf-8", b'{"ok":true}')
                except Exception as exc:
                    self._send_bytes(404, "text/plain; charset=utf-8", str(exc).encode("utf-8"))
                return

            upstream_path = f"{parsed.path}?{parsed.query}" if parsed.query else parsed.path
            if self.headers.get("Upgrade", "").lower() == "websocket":
                self._handle_websocket_proxy(upstream_path)
            else:
                self._proxy_http(upstream_path)

        def log_message(self, format, *args) -> None:
            print(f"[http] {self.address_string()} - {format % args}", file=sys.stderr)

    return Handler


def main() -> None:
    root = Path.cwd()
    previewer = Previewer(root=root, port=PREVIEW_PORT)
    shell_url = f"http://127.0.0.1:{SHELL_PORT}/shell"
    server = ThreadingHTTPServer(("127.0.0.1", SHELL_PORT), make_handler(previewer, HTML_SHELL))

    threading.Thread(target=server.serve_forever, daemon=True).start()

    typ_files = sorted(root.glob("*.typ"))
    if typ_files:
        previewer.switch_file(typ_files[0])
        wait_for_port("127.0.0.1", PREVIEW_PORT)

    subprocess.Popen(
        ["xdg-open", shell_url],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    print(f"Shell: {shell_url} | Proxying /* -> 127.0.0.1:{PREVIEW_PORT} (except /shell and /open)")

    try:
        threading.Event().wait()
    except KeyboardInterrupt:
        print("\nShutting down...")
    finally:
        server.shutdown()
        server.server_close()
        previewer.stop()


if __name__ == "__main__":
    main()
