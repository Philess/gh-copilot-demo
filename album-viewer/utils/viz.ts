import d3 from "d3";

// generate a plot with d3js of the selling price of the album by year
// x-axis are the month series and y-axis show the numbers of album selled
export const generatePlot = (data: any) => {
    const margin = { top: 20, right: 20, bottom: 30, left: 50 };
    const width = 960 - margin.left - margin.right;
    const height = 500 - margin.top - margin.bottom;
    const parseDate = d3.timeParse('%Y-%m-%d');
    const x = d3.scaleTime().range([0, width]);
    const y = d3.scaleLinear().range([height, 0]);
    const xAxis = d3.axisBottom(x).ticks(12);
    const yAxis = d3.axisLeft(y).ticks(10);
    const valueline = d3.line()
        .x((d: any) => x(d.date))
        .y((d: any) => y(d.close));
    const svg = d3.select('#plot').append('svg');
    const g = svg.append('g');
    svg.attr('width', width + margin.left + margin.right);
    svg.attr('height', height + margin.top + margin.bottom);
    g.attr('transform', `translate(${margin.left},${margin.top})`);
    data.forEach((d: any) => {
        d.date = parseDate(d.date);
        d.close = +d.close;
    });
    x.domain(d3.extent(data, (d: any) => d.date));
    y.domain([0, d3.max(data, (d: any) => d.close)]);
    g.append('g')
        .attr('transform', `translate(0,${height})`)
        .call(xAxis);
    g.append('g')
        .call(yAxis);
    g.append('path')
        .data([data])
        .attr('class', 'line')
        .attr('d', valueline);
}