using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using System.ComponentModel.DataAnnotations;
using System.Threading.Tasks;
using AutoMapper;
using FluentValidation;
using Serilog;

namespace UnsecureApp.Controllers
{
    [ApiController]
    [ApiVersion("1.0")]
    [Route("api/v{version:apiVersion}/[controller]")]
    [Authorize]
    public class MyController : ControllerBase
    {
        private readonly IProductRepository _productRepository;
        private readonly IFileService _fileService;
        private readonly ILogger<MyController> _logger;
        private readonly IMapper _mapper;
        private readonly IValidator<ReadFileRequest> _readFileValidator;

        public MyController(
            IProductRepository productRepository,
            IFileService fileService,
            ILogger<MyController> logger,
            IMapper mapper,
            IValidator<ReadFileRequest> readFileValidator)
        {
            _productRepository = productRepository;
            _fileService = fileService;
            _logger = logger;
            _mapper = mapper;
            _readFileValidator = readFileValidator;
        }

        [HttpPost("read-file")]
        public async Task<IActionResult> ReadFileAsync([FromBody] ReadFileRequest request)
        {
            var validationResult = await _readFileValidator.ValidateAsync(request);
            if (!validationResult.IsValid)
                return BadRequest(validationResult.Errors);

            try
            {
                var content = await _fileService.ReadFileAsync(request.FilePath);
                return Ok(new { content });
            }
            catch (FileNotFoundException ex)
            {
                _logger.LogWarning(ex, "File not found: {FilePath}", request.FilePath);
                return NotFound();
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error reading file");
                return StatusCode(500, "Internal server error");
            }
        }

        [HttpGet("product")]
        public async Task<IActionResult> GetProductAsync([FromQuery][Required][MaxLength(100)] string productName)
        {
            if (string.IsNullOrWhiteSpace(productName))
                return BadRequest("Product name is required.");

            try
            {
                var productId = await _productRepository.GetProductIdByNameAsync(productName);
                if (productId == null)
                    return NotFound();
                return Ok(productId);
            }
            catch (ProductDataAccessException ex)
            {
                _logger.LogError(ex, "Error retrieving product");
                return StatusCode(500, "Internal server error");
            }
        }
    }

    // DTOs and Validators
    public class ReadFileRequest
    {
        public string FilePath { get; set; }
    }

    public class ReadFileRequestValidator : AbstractValidator<ReadFileRequest>
    {
        public ReadFileRequestValidator()
        {
            RuleFor(x => x.FilePath)
                .NotEmpty()
                .Must(path =>
                {
                    var fullPath = Path.GetFullPath(path);
                    var safeDirectory = Path.GetFullPath("./safe-directory");
                    return fullPath.StartsWith(safeDirectory) && Path.GetRelativePath(safeDirectory, fullPath).IndexOf("..") == -1;
                })
                .WithMessage("Invalid file path.");
        }
    }

    // Custom Exception
    public class ProductDataAccessException : Exception
    {
        public ProductDataAccessException(string message, Exception inner) : base(message, inner) { }
    }

    // Repository Interface
    public interface IProductRepository
    {
        Task<int?> GetProductIdByNameAsync(string productName);
    }

    // File Service Interface
    public interface IFileService
    {
        Task<string> ReadFileAsync(string filePath);
    }
}