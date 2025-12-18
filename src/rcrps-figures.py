# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:nomarker
#     text_representation:
#       extension: .py
#       format_name: nomarker
#       format_version: '1.0'
#       jupytext_version: 1.18.1
#   kernelspec:
#     display_name: ms_figs
#     language: python
#     name: ms_figs
# ---

import pandas as pd
import numpy as np

from scipy.stats import bootstrap

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker
from matplotlib.ticker import NullFormatter


def concat_scores_tsv(root_directory):
    import os

    dataframes = []
    for dirpath, _, filenames in os.walk(root_directory):
        if "scores.tsv" in filenames:
            file_path = os.path.join(dirpath, "scores.tsv")
            try:
                df = pd.read_csv(file_path, sep="\t")
                dataframes.append(df)
            except Exception as e:
                print(f"Error reading {file_path}: {e}")
    return (
        pd.concat(dataframes, ignore_index=True, sort=False)
        if dataframes
        else pd.DataFrame()
    )


scores_file = "scores_raw_latest.parquet"

cols = ["location", "forecast_date"]
data_col = "rcrps_log"
stat = np.mean
rng = np.random.default_rng(seed=123)

# data_dir = '/mnt/d/eval_latest'
# df = concat_scores_tsv(data_dir)
# df.to_parquet(scores_file)

df_raw = pd.read_parquet(scores_file)

df = df_raw[df_raw.scale == "log"].copy()

cols_pivot = ["location", "forecast_date", "date"]

df = df.pivot(index=cols_pivot, columns="model", values="crps")
df.columns.name = None
df = df[df.hosp.notna() & df.ww.notna()].reset_index()

df[data_col] = np.log(df.ww / df.hosp)
df = df.groupby(cols, as_index=False)[data_col].agg("mean")

out = {}

for c in cols:
    out[c] = []
    for k, g in df.groupby(c):
        vals = g[data_col].to_numpy()
        res = bootstrap(
            (vals,), stat, confidence_level=0.95, rng=rng, method="percentile"
        )
        out[c].append(
            {
                c: k,
                "estimate": stat(vals),
                "ci_lo": res.confidence_interval.low,
                "ci_hi": res.confidence_interval.high,
                "n_vals": len(vals),
            }
        )

cols_log = ["estimate", "ci_lo", "ci_hi"]

df_loc = pd.DataFrame(out["location"])
df_loc[cols_log] = np.exp(df_loc[cols_log])

df_date = pd.DataFrame(out["forecast_date"])
df_date[cols_log] = np.exp(df_date[cols_log])
df_date["forecast_date"] = pd.to_datetime(df_date["forecast_date"])

df_plot = df_date.sort_values("forecast_date").copy()

x = df_plot["forecast_date"]
y = df_plot["estimate"]
y_lo = df_plot["ci_lo"]
y_hi = df_plot["ci_hi"]

fig, ax = plt.subplots(figsize=(8, 6))

ax.plot(x, y, linewidth=2)
ax.scatter(x, y, s=36)  # size ~ 6**2

yerr = np.vstack([y - y_lo, y_hi - y])
ax.errorbar(x, y, yerr=yerr, fmt="none", capsize=4, linewidth=1)

ax.set_yscale("log")
ax.set_ylim(0.5, 2)

ticks = [0.5, 1, 1.5, 2]
ax.set_yticks(ticks)
ax.set_yticklabels([str(t) for t in ticks])

ax.yaxis.set_minor_formatter(NullFormatter())

ax.yaxis.set_minor_locator(
    ticker.LogLocator(base=10, subs=np.arange(0.5, 2, 0.1))
)

ax.set_title("WW/Hosp ratio by forecast date (95% CI)")
ax.set_xlabel("Forecast Date")
ax.set_ylabel("WW / hospital ratio (geometric mean)")

ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
ax.xaxis.set_major_locator(mdates.AutoDateLocator())
plt.setp(ax.get_xticklabels(), rotation=45, ha="right")  # similar to 0.8 rad

ax.grid(which="major", linestyle="-", linewidth=0.6, alpha=0.7)

plt.tight_layout()

plt.savefig("ratio_forecast_date.png")

plt.show()


df_plot = df_loc.sort_values("estimate").copy()

x = df_plot["location"]
y = df_plot["estimate"]
y_lo = df_plot["ci_lo"]
y_hi = df_plot["ci_hi"]

fig, ax = plt.subplots(figsize=(14, 8))

# Line + scatter
# ax.plot(x, y, linewidth=2)
ax.scatter(x, y, s=36)  # size ~ 6**2

yerr = np.vstack([y - y_lo, y_hi - y])
ax.errorbar(x, y, yerr=yerr, fmt="none", capsize=4, linewidth=1)

ax.set_yscale("log")
ax.set_ylim(0.5, 2.5)

ticks = [0.5, 1, 1.5, 2, 2.5]
ax.set_yticks(ticks)
ax.set_yticklabels([str(t) for t in ticks])

ax.yaxis.set_minor_formatter(NullFormatter())

ax.yaxis.set_minor_locator(
    ticker.LogLocator(base=10, subs=np.arange(0.5, 2.5, 0.1))
)

ax.set_title("WW/Hosp ratio by location (95% CI)")
ax.set_xlabel("Location")
ax.set_ylabel("WW / hospital ratio (geometric mean)")

ax.grid(which="major", linestyle="-", linewidth=0.6, alpha=0.7)
ax.margins(x=0.01)


plt.tight_layout()

plt.savefig("ratio_location.png")

plt.show()


df_loc.estimate.mean()

df_date.estimate.mean()
