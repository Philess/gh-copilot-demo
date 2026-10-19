# GitHub Copilot documentation

This documentation is generated with GitHub Copilot to show what the tool can do.

##
## Album API

The Album API provides endpoints to retrieve and sort albums.

### Endpoints

- `GET /albums` - Retrieves all albums.
- `GET /albums/{id}` - Retrieves a specific album by ID.
- `GET /albums/sort?sortBy={criteria}` - Retrieves albums sorted by the specified criteria (title, artist, or price).   
### Example Usage
To retrieve all albums:
```bash
curl -X GET "http://localhost:5000/albums"
```
To retrieve a specific album by ID:
