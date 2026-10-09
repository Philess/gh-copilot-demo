import argparse
import json
from pathlib import Path
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import urlopen


def get_response(base_url, path):
    try:
        response = urlopen(f"{base_url.rstrip('/')}/{path}")
    except HTTPError as error:
        response = error

    with response:
        return (
            response.status,
            response.headers.get("Content-Type", ""),
            response.read(),
        )


def capture(base_url):
    list_status, content_type, body = get_response(base_url, "albums")
    if list_status != 200:
        raise RuntimeError(f"GET /albums returned HTTP {list_status}")

    result = {
        "list": {
            "status": list_status,
            "content_type": content_type,
            "body": json.loads(body),
        },
        "numeric_detail": [],
        "invalid_detail_id": [],
    }

    for album_id in (1, 6, 999):
        status, _, body = get_response(base_url, f"albums/{album_id}")
        result["numeric_detail"].append(
            {"id": album_id, "status": status, "body": body.decode("utf-8")}
        )

    for value in ("abc", "1.5", "2147483648"):
        status, _, _ = get_response(base_url, f"albums/{quote(value, safe='')}")
        result["invalid_detail_id"].append({"value": value, "status": status})

    return result


def main():
    parser = argparse.ArgumentParser(description="Capture the original albums API contract.")
    parser.add_argument("--base-url", default="http://127.0.0.1:3000")
    parser.add_argument("--output", type=Path, help="Write JSON to this path instead of stdout.")
    args = parser.parse_args()

    rendered = json.dumps(capture(args.base_url), indent=2) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    else:
        print(rendered, end="")


if __name__ == "__main__":
    main()
