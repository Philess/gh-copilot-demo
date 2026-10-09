import argparse
import json
from pathlib import Path


BASELINE = Path(__file__).with_name("api-baseline.json")
ALBUM_FIELDS = {"id", "title", "artist", "price", "image_url"}


def load_json(path):
    with path.open(encoding="utf-8") as file:
        return json.load(file)


def verify(capture, baseline):
    expected_albums = baseline["list"]["body"]
    actual_list = capture["list"]

    assert actual_list["status"] == baseline["list"]["status"], "GET /albums status changed"
    assert actual_list["content_type"].split(";", 1)[0].strip().lower() == "application/json", (
        "GET /albums must remain JSON"
    )
    assert actual_list["body"] == expected_albums, "GET /albums records differ from the baseline"
    assert [album["id"] for album in actual_list["body"]] == list(range(1, 7)), (
        "album IDs must be ascending from 1 through 6"
    )
    for album in actual_list["body"]:
        assert set(album) == ALBUM_FIELDS, f"unexpected JSON fields for album {album.get('id')}"
        assert isinstance(album["price"], (int, float)) and not isinstance(album["price"], bool), (
            f"price for album {album['id']} must be a JSON number"
        )

    assert capture["numeric_detail"] == baseline["numeric_detail"], (
        "numeric detail requests must retain their empty HTTP 200 responses"
    )
    actual_invalid = capture["invalid_detail_id"]
    expected_invalid = baseline["invalid_detail_id"]
    assert actual_invalid == expected_invalid, (
        "invalid-ID probes must retain the captured status codes"
    )


def main():
    parser = argparse.ArgumentParser(description="Assert a captured response against the catalog baseline.")
    parser.add_argument("--capture", type=Path, default=BASELINE)
    parser.add_argument("--baseline", type=Path, default=BASELINE)
    args = parser.parse_args()

    verify(load_json(args.capture), load_json(args.baseline))
    print("API compatibility contract passed.")


if __name__ == "__main__":
    main()
