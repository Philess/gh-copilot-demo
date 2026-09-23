import * as d3 from 'd3'

export interface SalesDataPoint {
  month: string
  sales: number
}

export interface SalesChartOptions {
  width?: number
  height?: number
  margin?: {
    top: number
    right: number
    bottom: number
    left: number
  }
}

const defaultOptions: Required<SalesChartOptions> = {
  width: 800,
  height: 400,
  margin: {
    top: 20,
    right: 30,
    bottom: 60,
    left: 60
  }
}

/**
 * Render monthly album sales as a bar chart.
 *
 * The target can be an HTMLElement or a D3 selection. Existing chart content
 * in the target is replaced so the function can be called after data updates.
 */
export function renderSalesChart(
  target: HTMLElement | d3.Selection<HTMLElement, unknown, null, undefined>,
  data: readonly SalesDataPoint[],
  options: SalesChartOptions = {}
): void {
  const config = {
    ...defaultOptions,
    ...options,
    margin: {
      ...defaultOptions.margin,
      ...options.margin
    }
  }
  const container = target instanceof HTMLElement ? d3.select(target) : target

  container.selectAll('*').remove()

  if (data.length === 0) {
    container
      .append('p')
      .attr('class', 'chart-empty')
      .text('No sales data available.')
    return
  }

  const innerWidth = config.width - config.margin.left - config.margin.right
  const innerHeight = config.height - config.margin.top - config.margin.bottom
  if (innerWidth <= 0 || innerHeight <= 0) {
    throw new Error('Chart dimensions must exceed the configured margins.')
  }

  const maxSales = d3.max(data, (point) => point.sales) ?? 0
  const x = d3
    .scaleBand<string>()
    .domain(data.map((point) => point.month))
    .range([0, innerWidth])
    .padding(0.1)
  const y = d3
    .scaleLinear()
    .domain([0, maxSales])
    .nice()
    .range([innerHeight, 0])

  const svg = container
    .append('svg')
    .attr('class', 'sales-chart')
    .attr('viewBox', `0 0 ${config.width} ${config.height}`)
    .attr('role', 'img')
    .attr('aria-label', 'Monthly album sales')

  const chart = svg
    .append('g')
    .attr('transform', `translate(${config.margin.left},${config.margin.top})`)

  chart
    .append('g')
    .attr('class', 'x-axis')
    .attr('transform', `translate(0,${innerHeight})`)
    .call(d3.axisBottom(x))
    .selectAll('text')
    .attr('transform', 'rotate(-35)')
    .style('text-anchor', 'end')

  chart
    .append('g')
    .attr('class', 'y-axis')
    .call(d3.axisLeft(y).ticks(5))

  chart
    .append('text')
    .attr('class', 'axis-label')
    .attr('x', innerWidth / 2)
    .attr('y', innerHeight + config.margin.bottom - 8)
    .style('text-anchor', 'middle')
    .text('Month')

  chart
    .append('text')
    .attr('class', 'axis-label')
    .attr('transform', 'rotate(-90)')
    .attr('x', -innerHeight / 2)
    .attr('y', -config.margin.left + 18)
    .style('text-anchor', 'middle')
    .text('Albums sold')

  chart
    .selectAll<SVGRectElement, SalesDataPoint>('.bar')
    .data(data)
    .join('rect')
    .attr('class', 'bar')
    .attr('x', (point) => x(point.month) ?? 0)
    .attr('y', (point) => y(point.sales))
    .attr('width', x.bandwidth())
    .attr('height', (point) => innerHeight - y(point.sales))
    .attr('aria-label', (point) => `${point.month}: ${point.sales} albums sold`)
}

export default renderSalesChart
