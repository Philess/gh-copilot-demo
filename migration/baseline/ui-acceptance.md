# Original catalog UI acceptance contract

The behavior below is read from the original executable Vue source. The original
page was not visually or interactively exercised in this Linux capture because
the available browser session could not authenticate. Do not label the source
review as a rendered-browser observation.

| Area | Acceptance contract | Source evidence | Capture status |
|---|---|---|---|
| Copy | Header: “🎵 Album Collection” and “Discover amazing music albums”. Loading copy: “Loading albums...”. API failure copy: “Failed to load albums. Please make sure the API is running.” Retry: “Try Again”. Card controls: “Add to Cart” and “Preview”. | `album-viewer/src/App.vue:4-5,9-16`; `album-viewer/src/components/AlbumCard.vue:24-25` | Source-derived; browser not exercised |
| Successful list | One card for each response record, keyed by `id`; each card displays title, artist, and `price.toFixed(2)` with a `$` prefix (two decimal places, USD-style formatting). | `album-viewer/src/App.vue:19-25`; `album-viewer/src/components/AlbumCard.vue:15-20` | Source-derived; browser not exercised |
| Loading/error/retry | Starts in loading state, fetches relative `/albums` on mount, clears errors on retry, displays the failure message on request rejection, and always leaves loading state in `finally`. Retry invokes the same fetch function. | `album-viewer/src/App.vue:9-17,36-56` | Source-derived; no browser/API-failure UI interaction |
| Genuine empty catalog | A successful `[]` response follows the normal `v-else` branch and renders an empty `.albums-grid`; there is no separate “no albums” message and no error state. | `album-viewer/src/App.vue:14-25` | Source-derived; no empty-response UI run |
| Images | Uses `image_url` as `src`, album title as `alt`, native `loading="lazy"`, and replaces a failed image with `https://via.placeholder.com/300x300/667eea/white?text=Album+Cover`. | `album-viewer/src/components/AlbumCard.vue:4-9,39-42` | Source-derived; image network/fallback not exercised |
| Keyboard behavior | Retry, Add to Cart, and Preview are native `<button>` elements and are keyboard-focusable/activatable, but Add to Cart and Preview have no action handlers. The hover play overlay is a `<div>` with no keyboard handler or button semantics; it is not a keyboard-operable control. | `album-viewer/src/App.vue:16`; `album-viewer/src/components/AlbumCard.vue:10-12,23-26` | Source-derived; no keyboard interaction test |
| Palette/layout | Body gradient runs from `#667eea` to `#764ba2`; cards and controls use the purple/indigo `#667eea` palette. Above 768px, the grid auto-fills columns with a 300px minimum. At `max-width: 768px`, the app uses 1rem padding, a 2rem heading, one grid column, and vertically stacked full-width card controls. | `album-viewer/index.html:8-13`; `album-viewer/src/App.vue:138-158`; `album-viewer/src/components/AlbumCard.vue:160-194` | Source-derived; neither viewport rendered |

There is no playback or cart implementation in the catalog component; the play
overlay and Add to Cart/Preview controls are presentation-only.
