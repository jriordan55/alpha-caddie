/** Parse PGA / metadata weather scalars ("71°F", "89%") and prefer Celsius when F is stale. */

export function parseWeatherScalar(v) {
  const s = String(v ?? "").trim();
  if (!s) return NaN;
  const direct = Number(s);
  if (Number.isFinite(direct)) return direct;
  const cleaned = s.replace(/[^0-9.-]+/g, "");
  const n = parseFloat(cleaned);
  return Number.isFinite(n) ? n : NaN;
}

export function celsiusToFahrenheit(c) {
  const n = Number(c);
  if (!Number.isFinite(n)) return NaN;
  return (n * 9) / 5 + 32;
}

function tempFFromRowFields(row) {
  const tempF = parseWeatherScalar(row?.pga_meta_weather_temp_f ?? row?.weather_temp_f);
  const tempC = parseWeatherScalar(row?.pga_meta_weather_temp_c ?? row?.weather_temp_c);
  const fromC = celsiusToFahrenheit(tempC);

  if (Number.isFinite(fromC) && Number.isFinite(tempF)) {
    const delta = Math.abs(tempF - fromC);
    if (delta <= 3) return tempF;
    // pgatouR often stores a stale overnight snapshot in weather_temp_f while C matches.
    if (delta > 8 && Number.isFinite(fromC)) return fromC;
    return fromC;
  }
  if (Number.isFinite(fromC)) return fromC;
  if (Number.isFinite(tempF)) return tempF;
  return NaN;
}

/** Metadata-only weather (tournament snapshot — may be wrong / same for all rounds). */
export function weatherFieldsFromMetadata(row) {
  const tempF = tempFFromRowFields(row);
  const windMph = parseWeatherScalar(row?.pga_meta_weather_wind_mph ?? row?.weather_wind_mph);
  const humidity = parseWeatherScalar(row?.pga_meta_weather_humidity ?? row?.weather_humidity);
  const condition = String(row?.pga_meta_weather_condition ?? row?.weather_condition ?? "").trim();
  return {
    weather_temp_f: Number.isFinite(tempF) ? Math.round(tempF * 10) / 10 : null,
    weather_wind_mph: Number.isFinite(windMph) ? Math.round(windMph * 10) / 10 : null,
    weather_humidity: Number.isFinite(humidity) ? Math.round(humidity) : null,
    weather_condition: condition || "",
  };
}

/** Prefer Open-Meteo per-round snapshot; fall back to fixed metadata. */
export function weatherFieldsForRound(row, roundWeatherByKey, roundKey) {
  const snap = roundWeatherByKey?.get?.(roundKey);
  if (snap && Number.isFinite(snap.tempF)) {
    return {
      weather_temp_f: Math.round(snap.tempF * 10) / 10,
      weather_wind_mph: Number.isFinite(snap.windMph) ? Math.round(snap.windMph * 10) / 10 : null,
      weather_humidity: Number.isFinite(snap.humidityPct) ? Math.round(snap.humidityPct) : null,
      weather_condition: String(snap.condition || "").toLowerCase(),
      weather_source: "open_meteo_archive",
    };
  }
  return { ...weatherFieldsFromMetadata(row), weather_source: "pga_metadata" };
}
