import app from './app'

const PORT = process.env.PORT ?? 3000

app.listen(PORT, () => {
  console.log(`Album API v2 listening on port ${PORT}`)
})
