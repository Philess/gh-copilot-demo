import argparse
import json
from pathlib import Path


BASELINE = Path(__file__).with_name("api-baseline.json")
ALBUM_FIELDS = {"id", "title", "artist", "price", "image_url"}


def load_json(path):
    with path.open(encoding="utf-8") as file:
        return json.load(file)


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def verify_album_records(albums):
    require(
        [album["id"] for album in albums] == list(range(1, 7)),
        "album IDs must be ascending from 1 through 6",
    )
    for album in albums:
        require(set(album) == ALBUM_FIELDS, f"unexpected JSON fields for album {album.get('id')}")
        require(type(album["id"]) is int, f"id for album {album['id']} must be an integer")
        require(isinstance(album["title"], str), f"title for album {album['id']} must be a string")
        require(isinstance(album["artist"], str), f"artist for album {album['id']} must be a string")
        require(
            isinstance(album["price"], (int, float)) and not isinstance(album["price"], bool),
            f"price for album {album['id']} must be a JSON number"
        )
        require(
            isinstance(album["image_url"], str),
            f"image_url for album {album['id']} must be a string"
        )


def verify_baseline(baseline):
    expected_list = baseline["list"]
    require(expected_list["status"] == 200, "baseline GET /albums status must be 200")
    require(
        expected_list["content_type"].split(";", 1)[0].strip().lower() == "application/json",
        "baseline GET /albums must be JSON",
    )

    expected_albums = baseline["list"]["body"]
    verify_album_records(expected_albums)

    require(
        [item["id"] for item in baseline["numeric_detail"]] == [1, 6, 999],
        "baseline detail probes must cover existing and nonexistent numeric IDs",
    )
    require(
        all(item["status"] == 200 and item["body"] == "" for item in baseline["numeric_detail"]),
        "numeric detail baseline must be empty HTTP 200",
    )
    require(
        [item["value"] for item in baseline["invalid_detail_id"]] == ["abc", "1.5", "2147483648"],
        "baseline invalid-ID probes must cover nonnumeric, fractional, and out-of-range values",
    )
    require(
        all(item["status"] == 400 for item in baseline["invalid_detail_id"]),
        "baseline invalid-ID status must be 400",
    )


def verify(capture, baseline):
    verify_baseline(baseline)
    actual_list = capture["list"]

    require(actual_list["status"] == 200, "GET /albums status changed")
    require(
        actual_list["content_type"].split(";", 1)[0].strip().lower() == "application/json",
        "GET /albums must remain JSON",
    )
    verify_album_records(actual_list["body"])
    require(actual_list["body"] == baseline["list"]["body"], "GET /albums records differ from the baseline")
    require(
        capture["numeric_detail"] == baseline["numeric_detail"],
        "numeric detail requests must retain their empty HTTP 200 responses",
    )
    require(
        capture["invalid_detail_id"] == baseline["invalid_detail_id"],
        "invalid-ID probes must retain the captured status codes",
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
