/// <reference types="vite/client" />
import React, { useEffect, useState } from 'react'
import AppBar from '@mui/material/AppBar'
import Box from '@mui/material/Box'
import CircularProgress from '@mui/material/CircularProgress'
import Container from '@mui/material/Container'
import Grid from '@mui/material/Grid2'
import Toolbar from '@mui/material/Toolbar'
import Typography from '@mui/material/Typography'
import Alert from '@mui/material/Alert'
import Button from '@mui/material/Button'
import axios from 'axios'
import AlbumCard from './components/AlbumCard'
import type { Album } from './types/album'

export default function App(): React.ReactElement {
  const [albums, setAlbums] = useState<Album[]>([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  const fetchAlbums = async (): Promise<void> => {
    setLoading(true)
    setError(null)
    try {
      const { data } = await axios.get<Album[]>('/albums')
      setAlbums(data)
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to fetch albums')
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    void fetchAlbums()
  }, [])

  const bgColor = import.meta.env.VITE_BACKGROUND_COLOR as string | undefined

  return (
    <Box
      sx={{
        minHeight: '100vh',
        background: bgColor ?? 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
      }}
    >
      <AppBar position="sticky" sx={{ backdropFilter: 'blur(8px)', background: 'rgba(0,0,0,0.45)' }} elevation={0}>
        <Toolbar>
          <Typography variant="h5" component="h1" fontWeight={700} letterSpacing={1}>
            🎵 Album Collection
          </Typography>
        </Toolbar>
      </AppBar>

      <Container maxWidth="xl" sx={{ py: 4 }}>
        {loading && (
          <Box sx={{ display: 'flex', justifyContent: 'center', py: 10 }}>
            <CircularProgress size={64} thickness={4} sx={{ color: 'white' }} />
          </Box>
        )}

        {!loading && error && (
          <Box sx={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 2, py: 6 }}>
            <Alert
              severity="error"
              sx={{ maxWidth: 480, width: '100%' }}
              action={
                <Button color="inherit" size="small" onClick={() => void fetchAlbums()}>
                  Retry
                </Button>
              }
            >
              {error}
            </Alert>
          </Box>
        )}

        {!loading && !error && (
          <Grid container spacing={3}>
            {albums.map((album) => (
              <Grid key={album.id} size={{ xs: 12, sm: 6, md: 4, lg: 3 }}>
                <AlbumCard album={album} />
              </Grid>
            ))}
          </Grid>
        )}
      </Container>
    </Box>
  )
}
