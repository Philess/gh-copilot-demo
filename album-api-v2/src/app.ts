import express from 'express'
import cors from 'cors'
import albumsRouter from './routes/albums'

const app = express()

app.use(cors())
app.use(express.json())

app.get('/', (_req, res) => {
  res.type('text/plain').send('Hit the /albums endpoint to retrieve a list of albums!')
})

app.use('/albums', albumsRouter)

export default app
