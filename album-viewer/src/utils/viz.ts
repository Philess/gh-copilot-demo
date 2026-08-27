import * as d3 from 'd3';

// load the data from json file and create the d3 svg in the then function
export function createD3Visualization(data: any[], svgElement: SVGSVGElement) {
    // create the svg
    const svg = d3.select(svgElement)
        .attr('width', 800)
        .attr('height', 600);
    
    const width = 800;
    const height = 600;

    // create the scales for the x and y axis
    // x-azis are the mobnt series and y-axis show the numbers of album selled
    const xScale = d3.scaleBand()
        .domain(data.map(d => d.month))
        .range([0, width])
        .padding(0.1);

    const yScale = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.albumsSold)])
        .range([height, 0]);

    // create axes for the x and y axis
    const xAxis = d3.axisBottom(xScale);
    const yAxis = d3.axisLeft(yScale);

    // generate a line chart based on the albums sales data
    const line = d3.line<any>()
        .x(d => xScale(d.month) + xScale.bandwidth() / 2)
        .y(d => yScale(d.albumsSold));
    svg.append('path')
        .datum(data)
        .attr('fill', 'none')
        .attr('stroke', 'steelblue')
        .attr('stroke-width', 2)
        .attr('d', line);

    // append the x and y axes to the svg
    svg.append('g')
        .attr('transform', `translate(0, ${height})`)
        .call(xAxis);

    svg.append('g')
        .call(yAxis);
}
