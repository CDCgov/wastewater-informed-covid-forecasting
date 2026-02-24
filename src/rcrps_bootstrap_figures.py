#!/usr/bin/env python3

import argparse
import os
import pandas as pd
import polars as pl
import numpy as np

from scipy.stats import bootstrap

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker
from matplotlib.ticker import NullFormatter
from pathlib import Path


def figure_bootstrapped_rcrps_by_forecast_date(
    df: pd.DataFrame, pointcolor: str = "darkgreen"
) -> tuple[plt.Figure, plt.Axes]:
    """
    Visualize boostrapped CI for rCRPS by
    forecast date.

    Parameters
    ----------
    df
        Pandas data frame of bootstrapped values.

    pointcolor
        Color for plotted points.

    Returns
    -------
    Tuple
        Containing the figure and axis objects.
    """
    df_plot = df.sort_values("forecast_date").copy()

    x = df_plot["forecast_date"]
    y = df_plot["estimate"]
    y_lo = df_plot["ci_lo"]
    y_hi = df_plot["ci_hi"]

    fig, ax = plt.subplots(figsize=(8, 6))

    ax.axhline(y=1, ls="dashed", lw=2, color="k")
    ax.plot(x, y, linewidth=2, color=pointcolor)
    ax.scatter(x, y, s=100, color=pointcolor)  # size ~ 6**2

    yerr = np.vstack([y - y_lo, y_hi - y])
    ax.errorbar(
        x, y, yerr=yerr, fmt="none", capsize=4, linewidth=2, color=pointcolor
    )

    ax.set_yscale("log")
    ax.set_ylim(0.5, 2)

    ticks = [0.5, 1, 1.5, 2]
    ax.set_yticks(ticks)
    ax.set_yticklabels([str(t) for t in ticks])

    ax.yaxis.set_minor_formatter(NullFormatter())

    ax.yaxis.set_minor_locator(
        ticker.LogLocator(base=10, subs=np.arange(0.5, 2, 0.1))
    )

    ax.set_title("rCRPS by forecast date with boostrapped 95% CI")
    ax.set_xlabel("Forecast Date")
    ax.set_ylabel("Relative CRPS")

    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
    ax.xaxis.set_major_locator(mdates.AutoDateLocator())
    plt.setp(
        ax.get_xticklabels(), rotation=45, ha="right"
    )  # similar to 0.8 rad
    ax.grid(which="major", linestyle="-", linewidth=0.6, alpha=0.7)

    fig.tight_layout()

    return fig, ax


def figure_bootstrapped_rcrps_by_location(
    df: pd.DataFrame, pointcolor: str = "darkgreen"
) -> tuple[plt.Figure, plt.Axes]:
    """
    Visualize boostrapped CI for rCRPS by
    location.

    Parameters
    ----------
    df
        Pandas data frame of bootstrapped values.

    pointcolor
        Color for plotted points.

    Returns
    -------
    Tuple
        Containing the figure and axis objects.
    """
    df_plot = df.sort_values("estimate").copy()

    x = df_plot["location"]
    y = df_plot["estimate"]
    y_lo = df_plot["ci_lo"]
    y_hi = df_plot["ci_hi"]

    fig, ax = plt.subplots(figsize=(14, 8))

    ax.axhline(y=1, lw=2, ls="dashed", color="k")
    ax.scatter(x, y, s=100, color=pointcolor)

    yerr = np.vstack([y - y_lo, y_hi - y])
    ax.errorbar(
        x, y, yerr=yerr, fmt="none", capsize=4, linewidth=2, color=pointcolor
    )

    ax.set_yscale("log")
    ax.set_ylim(1 / 2.5, 2.5)

    ticks = [0.5, 1, 1.5, 2, 2.5]
    ax.set_yticks(ticks)
    ax.set_yticklabels([str(t) for t in ticks])

    ax.yaxis.set_minor_formatter(NullFormatter())

    ax.yaxis.set_minor_locator(
        ticker.LogLocator(base=10, subs=np.arange(0.5, 2.5, 0.1))
    )

    ax.set_title("rCRPS by location with boostrapped 95% CI")
    ax.set_xlabel("Location")
    ax.set_ylabel("Relative CRPS")

    ax.grid(which="major", linestyle="-", linewidth=0.6, alpha=0.7)
    ax.margins(x=0.01)

    fig.tight_layout()

    return fig, ax


def rcrps_boostrap_figures(
    scores_file: Path, output_dir: Path, figext: str
) -> None:
    """
    Create figures of boostrapped rCRPS by
    forecast date and location from a raw
    scores file, and save them to disk.
    """

    cols = ["location", "forecast_date"]
    data_col = "log_mean_scores_ratio"
    stat = np.mean
    rng = np.random.default_rng(seed=123)
    pointcolor = "darkgreen"
    df = (
        pl.read_parquet(scores_file)
        .filter(pl.col("model") == "cfa-wwrenewal(retro)")
        .with_columns(log_mean_scores_ratio=pl.col("mean_scores_ratio").log())
        .select("forecast_date", "location", data_col)
        .to_pandas()
    )

    out = {}

    for c in cols:
        out[c] = []
        for k, g in df.groupby(c):
            vals = g[data_col].to_numpy()
            res = bootstrap(
                (vals,),
                stat,
                confidence_level=0.95,
                method="percentile",
                rng=rng,
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

    df_date = pd.DataFrame(out["forecast_date"])
    df_date[cols_log] = np.exp(df_date[cols_log])
    df_date["forecast_date"] = pd.to_datetime(df_date["forecast_date"])
    fig_date, ax_date = figure_bootstrapped_rcrps_by_forecast_date(
        df_date, pointcolor=pointcolor
    )

    df_loc = pd.DataFrame(out["location"])
    df_loc[cols_log] = np.exp(df_loc[cols_log])
    fig_loc, ax_loc = figure_bootstrapped_rcrps_by_location(
        df_loc, pointcolor=pointcolor
    )

    os.makedirs(output_dir, exist_ok=True)
    fig_date.savefig(
        Path(output_dir / "boostrapped_rcprs_by_forecast_date").with_suffix(
            figext
        )
    )

    fig_loc.savefig(
        Path(output_dir / "boostrapped_rcprs_by_location").with_suffix(figext)
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=("Produce boostrapped rCRPS figures.")
    )
    parser.add_argument(
        "scores_path",
        type=Path,
        help=(
            "Path to a parquet file containing raw rCRPS values "
            "for each location/forecast date pair"
        ),
    )
    parser.add_argument(
        "output_dir",
        type=Path,
        help=("Path to an output directory for the figures"),
    )
    parser.add_argument(
        "figext", type=str, help="Output format for the figures", default="png"
    )
    args = parser.parse_args()

    rcrps_boostrap_figures(args.scores_path, args.output_dir, args.figext)
