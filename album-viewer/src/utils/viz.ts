import * as d3 from "d3";

// load the data from a json file and create the d3 svg in the then function
export function createViz() {
    d3.json("/data/albums.json").then((data) => {
        // create the svg
        const svg = d3.select("#viz")
            .append("svg")
            .attr("width", 800)
            .attr("height", 600);

        // create the scales for the x and y axis
        // x-axis are the month series and y-axis show the numbers of album selled
        const xScale = d3.scaleBand()
            .domain(data.map((d) => d.month))
            .range([0, 800])
            .padding(0.1);

        const yScale = d3.scaleLinear()
            .domain([0, d3.max(data, (d) => d.sales)!])
            .range([600, 0]);
        
        // create axes for the x and y axis
        const xAxis = d3.axisBottom(xScale);
        const yAxis = d3.axisLeft(yScale);

        // add the x and y axis to the svg
        svg.append("g")
            .attr("transform", "translate(0, 600)")
            .call(xAxis);

        svg.append("g")
            .call(yAxis);
            
        // generate a line chart based on the albums sales data
        const line = d3.line()
            .x((d) => xScale(d.month)! + xScale.bandwidth() / 2)
            .y((d) => yScale(d.sales));

        svg.append("path")
            .datum(data)
            .attr("fill", "none")
            .attr("stroke", "steelblue")
            .attr("stroke-width", 1.5)
            .attr("d", line);   
    });
}