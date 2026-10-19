// generate a plot with D3.js of the selling price of the album by year
// x-axis are the month series and y-axis show the numbers of albums sold
// data from the sales of album are loaded in from an external source and are in json format
// example data format:
// [
//   { "year": 2020, "month": "January", "sales": 120 },
//   { "year": 2020, "month": "February", "sales": 150 },
//   ...
// ]

import * as d3 from 'd3';

// load the data from a json file and create the d3 svg in the then function
d3.json('path/to/data.json').then((data: { year: number; month: string; sales: number }[]) => {
  // create the svg
  const margin = { top: 20, right: 30, bottom: 40, left: 50 };
  const width = 800 - margin.left - margin.right;
  const height = 400 - margin.top - margin.bottom;

  const svg = d3.select('body')
    .append('svg')
    .attr('width', width + margin.left + margin.right)
    .attr('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left},${margin.top})`);

    // create the scales for the x and y axis
    // x-axis are the month series and y-axis show the numbers of album selled
    const x = d3.scaleBand()
      .domain(data.map(d => d.month))
      .range([0, width])
      .padding(0.1);

    const y = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.sales) || 0])
      .nice()
      .range([height, 0]);

    // create the axes for the x and y axis
    svg.append('g')
      .attr('transform', `translate(0,${height})`)
      .call(d3.axisBottom(x));

    svg.append('g')
      .call(d3.axisLeft(y));

   // generate a line chart based on the albums sales data
    const line = d3.line<{ month: string; sales: number }>()
      .x(d => (x(d.month) || 0) + x.bandwidth() / 2)
      .y(d => y(d.sales));

    svg.append('path')
      .datum(data)
      .attr('fill', 'none')
      .attr('stroke', 'steelblue')
      .attr('stroke-width', 1.5)
      .attr('d', line);

});
