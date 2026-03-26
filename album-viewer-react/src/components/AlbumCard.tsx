import React from 'react'
import Card from '@mui/material/Card'
import CardMedia from '@mui/material/CardMedia'
import CardContent from '@mui/material/CardContent'
import Typography from '@mui/material/Typography'
import Chip from '@mui/material/Chip'
import type { Album } from '../types/album'

interface AlbumCardProps {
  album: Album
}

export default function AlbumCard({ album }: AlbumCardProps): React.ReactElement {
  return (
    <Card
      sx={{
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        transition: 'transform 0.2s ease, box-shadow 0.2s ease',
        '&:hover': {
          transform: 'translateY(-4px)',
          boxShadow: 8,
        },
      }}
      elevation={3}
    >
      <CardMedia
        component="img"
        image={album.image_url}
        alt={`${album.title} cover`}
        sx={{ aspectRatio: '1 / 1', objectFit: 'cover' }}
      />
      <CardContent sx={{ flexGrow: 1, display: 'flex', flexDirection: 'column', gap: 0.5 }}>
        <Typography variant="h6" component="h2" fontWeight={600} noWrap title={album.title}>
          {album.title}
        </Typography>
        <Typography variant="body2" color="text.secondary" noWrap title={album.artist}>
          {album.artist}
        </Typography>
        <Chip
          label={`$${album.price.toFixed(2)}`}
          color="primary"
          size="small"
          sx={{ alignSelf: 'flex-start', mt: 'auto', fontWeight: 600 }}
        />
      </CardContent>
    </Card>
  )
}
