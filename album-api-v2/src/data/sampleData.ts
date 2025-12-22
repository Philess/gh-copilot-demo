import { Album } from '../models/album';

export const albums: Album[] = [
  {
    Id: 1,
    Title: 'You, Me and an App Id',
    Artist: {
      Name: 'Daprize',
      Birthdate: '2015-05-10T00:00:00',
      BirthPlace: 'Seattle'
    },
    Year: 2020,
    Price: 10.99,
    Image_url: 'https://aka.ms/albums-daprlogo'
  },
  {
    Id: 2,
    Title: 'Seven Revision Army',
    Artist: {
      Name: 'The Blue-Green Stripes',
      Birthdate: '2010-03-15T00:00:00',
      BirthPlace: 'Portland'
    },
    Year: 2021,
    Price: 13.99,
    Image_url: 'https://aka.ms/albums-containerappslogo'
  },
  {
    Id: 3,
    Title: 'Scale It Up',
    Artist: {
      Name: 'KEDA Club',
      Birthdate: '2018-07-20T00:00:00',
      BirthPlace: 'San Francisco'
    },
    Year: 2022,
    Price: 13.99,
    Image_url: 'https://aka.ms/albums-kedalogo'
  },
  {
    Id: 4,
    Title: 'Lost in Translation',
    Artist: {
      Name: 'MegaDNS',
      Birthdate: '2012-01-08T00:00:00',
      BirthPlace: 'Austin'
    },
    Year: 2023,
    Price: 12.99,
    Image_url: 'https://aka.ms/albums-envoylogo'
  },
  {
    Id: 5,
    Title: 'Lock Down Your Love',
    Artist: {
      Name: 'V is for VNET',
      Birthdate: '2019-09-12T00:00:00',
      BirthPlace: 'Denver'
    },
    Year: 2024,
    Price: 12.99,
    Image_url: 'https://aka.ms/albums-vnetlogo'
  },
  {
    Id: 6,
    Title: "Sweet Container O' Mine",
    Artist: {
      Name: 'Guns N Probeses',
      Birthdate: '2008-11-03T00:00:00',
      BirthPlace: 'Los Angeles'
    },
    Year: 2025,
    Price: 14.99,
    Image_url: 'https://aka.ms/albums-containerappslogo'
  }
];
