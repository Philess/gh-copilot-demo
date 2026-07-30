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

app.UseHttpsRedirection();
app.UseAuthorization();
app.UseRouting();


app.MapGet("/", async context =>
{
    await context.Response.WriteAsync("Hit the /albums endpoint to retrieve a list of albums!");
});

// Redirect exact /album -> /albums
app.MapGet("/album", context =>
{
    context.Response.Redirect("/albums", permanent: true); // 301
    return Task.CompletedTask;
});

// Redirect /album/... -> /albums/...
app.MapGet("/album/{*rest}", context =>
{
    var rest = context.Request.RouteValues["rest"]?.ToString() ?? string.Empty;
    var suffix = string.IsNullOrEmpty(rest) ? "" : "/" + rest;
    var target = $"/albums{suffix}{context.Request.QueryString}";
    context.Response.Redirect(target, permanent: true);
    return Task.CompletedTask;
});

app.MapControllers();

app.Run();
