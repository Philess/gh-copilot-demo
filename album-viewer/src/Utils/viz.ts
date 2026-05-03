// generate a plot with D3.js of the selling price of the album by year
// x-axis are the month series and y-axis show the numbers of albums sold
// data from the sales of album are loaded in from an external source and are in json format
/*import * as d3 from 'd3'; // ensure d3 is installed: npm install d3 @types/d3

export function generatePlot(data: { month: string; year: number; sales: number }[]) {  
    // set the dimensions and margins of the graph  
    const margin = { top: 20, right: 30, bottom: 40, left: 50 },
        width = 800 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;  
    // append the svg object to the body of the page
    const svg = d3.select("#plot")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);
    // X axis
    const x = d3.scaleBand()
        .range([0, width])
        .domain(data.map(d => `${d.month} ${d.year}`))
        .padding(0.2);
    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x))
        .selectAll("text")
        .attr("transform", "translate(-10,0)rotate(-45)")
        .style("text-anchor", "end");   
    // Y axis
    const y = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.sales) || 0])
        .range([height, 0]);
    svg.append("g")
        .call(d3.axisLeft(y));
    // Bars
    svg.selectAll("mybar")
        .data(data)
        .enter()
        .append("rect")
        .attr("x", d => x(`${d.month} ${d.year}`) || 0)
        .attr("y", d => y(d.sales))
        .attr("width", x.bandwidth())
        .attr("height", d => height - y(d.sales))
        .attr("fill", "#69b3a2");
}*/


import * as d3 from "d3";

// load the data from a json file and create the d3 svg in the then function
export function loadDataAndGeneratePlot(jsonFilePath: string) {
    d3.json(jsonFilePath).then((data) => {
        generatePlot(data as { month: string; year: number; sales: number }[]);
    }).catch(error => {
        console.error("Error loading data:", error);
    });
}

function generatePlot(data: { month: string; year: number; sales: number }[]) {
    // create the svg
// create the scales for the x and y axis
// x-axis are the month series and y-axis show the numbers of album selled

    const margin = { top: 20, right: 30, bottom: 40, left: 50 },
        width = 800 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;
    const svg = d3.select("#plot")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);
    const x = d3.scaleBand()
        .range([0, width])
        .domain(data.map(d => `${d.month} ${d.year}`))
        .padding(0.2);
    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x))
        .selectAll("text")
        .attr("transform", "translate(-10,0)rotate(-45)")
        .style("text-anchor", "end");
    const y = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.sales) || 0])
        .range([height, 0]);
    svg.append("g")
        .call(d3.axisLeft(y));
    svg.selectAll("mybar")
        .data(data)
        .enter()
        .append("rect")
        .attr("x", d => x(`${d.month} ${d.year}`) || 0)
        .attr("y", d => y(d.sales))
        .attr("width", x.bandwidth())
        .attr("height", d => height - y(d.sales))
        .attr("fill", "#69b3a2");

}   




