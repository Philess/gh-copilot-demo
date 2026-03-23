import type { Album } from '../types.js'

export const sampleAlbums: Album[] = [
  {
    id: 1,
    title: 'You, Me and an App Id',
    artist: {
      name: 'Daprize',
      birthdate: '1988-05-14',
      birthPlace: 'Seattle, USA',
    },
    year: 2020,
    price: 10.99,
    image_url: 'https://aka.ms/albums-daprlogo',
  },
  {
    id: 2,
    title: 'Seven Revision Army',
    artist: {
      name: 'The Blue-Green Stripes',
      birthdate: '1992-08-21',
      birthPlace: 'Portland, USA',
    },
    year: 2021,
    price: 13.99,
    image_url: 'https://aka.ms/albums-containerappslogo',
  },
  {
    id: 3,
    title: 'Scale It Up',
    artist: {
      name: 'KEDA Club',
      birthdate: '1985-11-03',
      birthPlace: 'Austin, USA',
    },
    year: 2022,
    price: 13.99,
    image_url: 'https://aka.ms/albums-kedalogo',
  },
  {
    id: 4,
    title: 'Lost in Translation',
    artist: {
      name: 'MegaDNS',
      birthdate: '1990-02-17',
      birthPlace: 'Dublin, Ireland',
    },
    year: 2020,
    price: 12.99,
    image_url: 'https://aka.ms/albums-envoylogo',
  },
  {
    id: 5,
    title: 'Lock Down Your Love',
    artist: {
      name: 'V is for VNET',
      birthdate: '1987-07-09',
      birthPlace: 'London, UK',
    },
    year: 2021,
    price: 12.99,
    image_url: 'https://aka.ms/albums-vnetlogo',
  },
  {
    id: 6,
    title: "Sweet Container O' Mine",
    artist: {
      name: 'Guns N Probeses',
      birthdate: '1994-01-28',
      birthPlace: 'Toronto, Canada',
    },
    year: 2022,
    price: 14.99,
    image_url: 'https://aka.ms/albums-containerappslogo',
  },
]