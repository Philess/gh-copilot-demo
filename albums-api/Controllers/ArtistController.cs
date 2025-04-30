using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

namespace albums_api.Controllers
{
    [Route("artists")]
    [ApiController]
    public class ArtistController : ControllerBase
    {
        /// <summary>
        /// Gets all artists.
        /// </summary>
        [HttpGet]
        public IActionResult Get()
        {
            var artists = Artist.GetAll();
            return Ok(artists);
        }
    }
}
