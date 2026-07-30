var builder = WebApplication.CreateBuilder(args);

var DefaultHttpPort = Environment.GetEnvironmentVariable("DAPR_HTTP_PORT") ?? "3500";
var AlbumStateStore = "statestore";
var CollectionId = Environment.GetEnvironmentVariable("COLLECTION_ID") ?? "GreatestHits";

// Add services to the container.

builder.Services.AddControllers();
// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();
builder.Services.AddHttpClient();

builder.Services.AddCors(options => {
    options.AddDefaultPolicy(builder =>
    {
        builder.AllowAnyOrigin();
        builder.AllowAnyHeader();
        builder.AllowAnyMethod();
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}


app.UseCors();

// Serve static files from wwwroot (can host a favicon.ico to avoid browser 404s)
app.UseStaticFiles();

// app.Urls.Add("${ASPNETCORE_URLS}");


app.UseHttpsRedirection();
app.UseRouting();

app.UseAuthorization();



// Health endpoint for platform health probes. Configure App Service or other platforms to use /health or /healthz
app.MapGet("/health", () => Results.Ok(new { status = "Healthy" }));
app.MapGet("/healthz", () => Results.Ok());

// Minimal handler for favicon to avoid browser-triggered 404s. Returns 204 No Content.
app.MapGet("/favicon.ico", (HttpContext ctx) =>
{
    ctx.Response.StatusCode = StatusCodes.Status204NoContent;
    return Task.CompletedTask;
});



app.MapGet("/", async context =>
{
    await context.Response.WriteAsync("Hit the /albums endpoint to retrieve a list of albums!");
});

app.MapControllers();

app.Run();
