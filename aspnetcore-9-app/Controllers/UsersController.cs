using Microsoft.AspNetCore.Mvc;

namespace aspnetcore_9_app.Controllers
{
    public class UsersController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }
    }
}