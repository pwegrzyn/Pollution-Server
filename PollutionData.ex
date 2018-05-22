defmodule PollutionData do

  def importLinesFromCSV(path) do
    lines = File.read!(path) |> String.split()
    IO.puts "Read #{length(lines)} lines of data"
    lines
  end

  def convertLine(line) do
    [date, time, x_coord, y_coord, val] = String.split(line, ",")
    map = %{}
    date_parsed = date |> String.split("-") |> Enum.reverse() |>
      Enum.map( &Integer.parse/1 ) |> Enum.map(& elem(&1, 0)) |>
      :erlang.list_to_tuple()
    time_parsed = time |> String.split(":") |> (& (&1 ++ ["0"])).() |>
      Enum.map( & Integer.parse(&1) |> elem(0) ) |> :erlang.list_to_tuple()
    coords = {x_coord |> Float.parse() |> elem(0), y_coord |> Float.parse() |> elem(0)}
    val_parsed = val |> Integer.parse() |> elem(0)
    map = Map.put(map, :location, coords)
    map = Map.put(map, :datetime, {date_parsed, time_parsed})
    map = Map.put(map, :pollutionLevel, val_parsed)
    map
  end

  def convertLines(lines) do
    converted = lines |> Enum.map( & PollutionData.convertLine(&1) )
    converted
  end

  def indentifyStations(list) do
    stations = list |> Enum.reduce(%{}, fn reading, acc ->
      Map.put(acc, "station_#{elem(reading[:location], 0)}_#{elem(reading[:location], 1)}", {elem(reading[:location], 0), elem(reading[:location], 1)} ) end)
    stations
  end

  def loadStations(stations) do
    for {st_iden, coords} <- stations, do: :pollution_gen_server.addStation(st_iden, coords)
  end

  def loadStationsWithTimer(stations) do
    function = fn -> loadStations(stations) end
    time = function |> :timer.tc |> elem(0)
    IO.puts "The loading of stations took #{time / 1000000} seconds"
  end

  def loadMeasuremets(data) do
    for m <- data, do: :pollution_gen_server.addValue(m[:location], m[:datetime], "PM10", m[:pollutionLevel])
  end

  def loadMeasuremetsWithTimer(data) do
    function = fn -> loadMeasuremets(data) end
    time = function |> :timer.tc |> elem(0)
    IO.puts "The loading of data took #{time / 1000000} seconds"
  end

  def getStationMeanWithTimer(coords, type) do
    function = fn -> :pollution_gen_server.getStationMean(coords, type) end
    {time, val} = function |> :timer.tc
    IO.puts "Getting the station mean took #{time / 1000000} seconds (#{val})"
  end

  def getDailyMeanWithTimer(day, type) do
    function = fn -> :pollution_gen_server.getDailyMean(day, type) end
    {time, val} = function |> :timer.tc
    IO.puts "Getting the daily mean took #{time / 1000000} seconds (#{val})"
  end

  def getMaximumGrowthTimeWithTimer(type) do
    function = fn -> :pollution_gen_server.getMaximumGrowthTime(type) end
    {time, val} = function |> :timer.tc
    {hour, {year, month, day}} = val
    IO.puts "Getting the maximum growth time took #{time / 1000000} seconds (#{hour}@#{day}-#{month}-#{year})"
  end

  def run() do
    :pollution_supervisor.start_link()
    lines = importLinesFromCSV("pollution.csv")
    lines = convertLines(lines)
    stations = indentifyStations(lines)
    loadStationsWithTimer(stations)
    loadMeasuremetsWithTimer(lines)
    getStationMeanWithTimer({20.06, 49.986}, "PM10")
    getDailyMeanWithTimer({2017, 5, 3}, "PM10")
    getMaximumGrowthTimeWithTimer("PM10")
  end

end
