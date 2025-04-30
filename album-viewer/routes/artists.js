const express = require("express");
const router = express.Router();
const axios = require("axios");

const DaprHttpPort = process.env.DAPR_HTTP_PORT || "3500";
const ArtistService = process.env.ALBUM_API_NAME || "album-api";
const Background = process.env.BACKGROUND_COLOR || "black";

/**
 * GET /artists - Render the artists page by fetching artists from the backend API.
 */
router.get("/", async function (req, res, next) {
  try {
    // Fetch artists from the backend API via Dapr
    const url = `http://127.0.0.1:${DaprHttpPort}/v1.0/invoke/${ArtistService}/method/artists`;
    const response = await axios.get(url, { headers: { "Content-Type": "application/json" } });
    const data = response.data || [];
    res.render("artists", {
      artists: data,
      background_color: Background,
    });
  } catch (err) {
    next(err);
  }
});

module.exports = router;
