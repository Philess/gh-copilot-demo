import * as d3 from "d3";
// load the data from a json file and create the d3 svg in the then function
export function createViz() {
  d3.json("/albums.json").then((data) => {
    const svg = d3.select("#viz").append("svg").attr("width", 500).attr("height", 500);
    // create the svg
    // create the scaless for the x and y axis
    // x-axis are the month series and y-axis show the numbers of albums sold
    const xScale = d3.scaleBand().domain(data.map((d) => d.month)).range([0, 500]).padding(0.1);
    const yScale = d3.scaleLinear().domain([0, d3.max(data, (d) => d.albums_sold)]).range([500, 0]);
    // create the bars for the bar chart
    svg
      .selectAll(".bar")
      .data(data)
      .enter()
      .append("rect")
      .attr("class", "bar")
      .attr("x", (d) => xScale(d.month)!)
      .attr("y", (d) => yScale(d.albums_sold))
      .attr("width", xScale.bandwidth())
      .attr("height", (d) => 500 - yScale(d.albums_sold));
    // add x-axis and y-axis
    svg.append("g").attr("transform", "translate(0,500)").call(d3.axisBottom(xScale));
    svg.append("g").call(d3.axisLeft(yScale));
  });
}
