"""Figures of the stability study, from the CSVs in ./out to ./figures."""
import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
import sys
TAG = sys.argv[1] if len(sys.argv) > 1 else "10k"
OUT = os.path.join(HERE, f"out_{TAG}")
FIG = os.path.join(HERE, f"figures_{TAG}")
os.makedirs(FIG, exist_ok=True)

ASSETS = ["Euro Govt", "Euro Corp IG", "Euro Corp HY", "Europe Equity"]
COL = {"Euro Govt": "#2a78d6", "Euro Corp IG": "#eb6834",
       "Euro Corp HY": "#1baf7a", "Europe Equity": "#eda100"}
FREQ_COL = {"daily": "#2a78d6", "weekly": "#eb6834"}
FREQ_LAB = {"daily": "giornaliero", "weekly": "settimanale"}

plt.rcParams.update({
    "font.family": "Arial", "font.size": 9, "axes.titlesize": 10, "axes.titleweight": "bold",
    "axes.spines.top": False, "axes.spines.right": False, "axes.grid": True,
    "grid.color": "#e6e6e6", "grid.linewidth": 0.6, "axes.edgecolor": "#888888",
    "legend.frameon": False, "figure.dpi": 150,
})

m = pd.read_csv(os.path.join(OUT, "monthly_runs.csv"))
noise = pd.read_csv(os.path.join(OUT, "noise_seeds.csv"))
seg = pd.read_csv(os.path.join(OUT, "govt_ig_segment.csv"))
real = pd.read_csv(os.path.join(OUT, "realised_forward.csv"))
months = list(m["month"].unique())
x = range(len(months))
xt = [mm[2:] for mm in months]          # "25-10"


def seed_range(month, freq, col):
    g = noise[(noise.month == month) & (noise.freq == freq)][col]
    return g.min(), g.max()


# --- 1. weights over time ---------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(9, 6.2), sharex=True)
for i, (tag, title) in enumerate([("minES", "Min-ES"), ("maxR", "Max-ratio")]):
    for j, freq in enumerate(["daily", "weekly"]):
        ax = axes[i, j]
        d = m[m.freq == freq]
        for a in ASSETS:
            ax.plot(x, 100 * d[f"{tag}_{a}"].values, color=COL[a], lw=2, marker="o", ms=3.5,
                    label=a)
            for k, mon in [(0, months[0]), (len(months) - 1, months[-1])]:
                lo, hi = seed_range(mon, freq, f"{tag}_{a}")
                ax.plot([k + 0.25] * 2, [100 * lo, 100 * hi], color=COL[a], lw=4,
                        alpha=0.35, solid_capstyle="butt")
        ax.set_title(f"{title}, rendimenti {FREQ_LAB[freq]}")
        ax.set_ylim(0, 85)
        if j == 0:
            ax.set_ylabel("peso (%)")
        ax.set_xticks(list(x))
        ax.set_xticklabels(xt, rotation=90)
axes[0, 0].legend(loc="upper right", ncol=2, fontsize=8)
fig.text(0.5, 0.005, "Barre spesse: intervallo dei pesi su 6 seed Monte Carlo alla prima e all'ultima data.",
         ha="center", fontsize=8, color="#555555")
fig.tight_layout(rect=(0, 0.02, 1, 1))
fig.savefig(os.path.join(FIG, "weights.png"))
plt.close(fig)

# --- 2. metrics over time -----------------------------------------------------
panels = [("minES_es", "Min-ES: ES 95% (%)", 100), ("minES_ret", "Min-ES: E[R] (%)", 100),
          ("maxR_ratio", "Max-ratio: rapporto E[R]/ES", 1), ("maxR_es", "Max-ratio: ES 95% (%)", 100)]
fig, axes = plt.subplots(2, 2, figsize=(9, 5.8), sharex=True)
for ax, (col, title, scale) in zip(axes.flat, panels):
    for freq in ["daily", "weekly"]:
        d = m[m.freq == freq]
        ax.plot(x, scale * d[col].values, color=FREQ_COL[freq], lw=2, marker="o", ms=3.5,
                label=FREQ_LAB[freq])
        lo, hi = seed_range(months[-1], freq, col)
        ax.axhspan(scale * lo, scale * hi, color=FREQ_COL[freq], alpha=0.12, lw=0)
    ax.set_title(title)
    ax.set_xticks(list(x))
    ax.set_xticklabels(xt, rotation=90)
axes[0, 0].legend(loc="lower left", fontsize=8)
fig.text(0.5, 0.005, "Bande: intervallo su 6 seed Monte Carlo alla data finale (ott 2026), stesso colore della serie.",
         ha="center", fontsize=8, color="#555555")
fig.tight_layout(rect=(0, 0.02, 1, 1))
fig.savefig(os.path.join(FIG, "metrics.png"))
plt.close(fig)

# --- 3. ES along the Govt/IG segment ----------------------------------------
fig, ax = plt.subplots(figsize=(6.2, 3.4))
for freq in ["daily", "weekly"]:
    d = seg[seg.freq == freq]
    ax.plot(100 * d.w_govt, 100 * d.es, color=FREQ_COL[freq], lw=2, label=FREQ_LAB[freq])
    j = d.es.idxmin()
    ax.plot(100 * d.w_govt[j], 100 * d.es[j], "o", color=FREQ_COL[freq], ms=7,
            mec="white", mew=1.5)
ax.set_xlabel("peso Euro Govt (%), il resto in Euro Corp IG; HY ed Equity a zero")
ax.set_ylabel("ES 95% a 60 giorni (%)")
ax.set_title("Quanto è piatta la valle dell'ES (dati a ott 2026)")
ax.legend(fontsize=8)
fig.tight_layout()
fig.savefig(os.path.join(FIG, "segment.png"))
plt.close(fig)

# --- 4. asset statistics over time -----------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(9, 5.8), sharex=True)
for j, freq in enumerate(["daily", "weekly"]):
    d = m[m.freq == freq]
    for i, (pre, title) in enumerate([("mean", "E[R] a 60 giorni per asset (%)"),
                                       ("es", "ES 95% a 60 giorni per asset (%)")]):
        ax = axes[i, j]
        for a in ASSETS:
            ax.plot(x, 100 * d[f"{pre}_{a}"].values, color=COL[a], lw=2, marker="o", ms=3.5,
                    label=a)
        ax.set_title(f"{title}, {FREQ_LAB[freq]}")
        ax.set_xticks(list(x))
        ax.set_xticklabels(xt, rotation=90)
axes[0, 0].legend(loc="center left", fontsize=8)
fig.tight_layout()
fig.savefig(os.path.join(FIG, "assets.png"))
plt.close(fig)

# --- 5. realised forward return vs expected and ES --------------------------
fig, axes = plt.subplots(1, 2, figsize=(9, 3.4), sharey=True)
for ax, (tag, title) in zip(axes, [("minES", "Min-ES"), ("maxR", "Max-ratio")]):
    for freq, off in [("daily", -0.17), ("weekly", 0.17)]:
        d = real[real.freq == freq].reset_index(drop=True)
        xs = [months.index(mm) + off for mm in d.month]
        ax.bar(xs, 100 * d[f"{tag}_real"], width=0.32, color=FREQ_COL[freq], label=f"realizzato, {FREQ_LAB[freq]}")
        ax.plot(xs, 100 * d[f"{tag}_exp"], "_", color=FREQ_COL[freq], ms=9, mew=2)
        ax.plot(xs, -100 * d[f"{tag}_es"], "_", color="#444444", ms=9, mew=1.2)
    ax.axhline(0, color="#888888", lw=0.8)
    ax.set_title(f"{title}: rendimento realizzato nei 60 giorni successivi")
    ax.set_xticks(list(x))
    ax.set_xticklabels(xt, rotation=90)
axes[0].set_ylabel("rendimento a 60 giorni (%)")
axes[0].legend(fontsize=8, loc="lower left")
fig.text(0.5, 0.005, "Trattini colorati: E[R] atteso dal run; trattini neri: -ES 95% del run. Ultimi tre mesi senza orizzonte completo.",
         ha="center", fontsize=8, color="#555555")
fig.tight_layout(rect=(0, 0.03, 1, 1))
fig.savefig(os.path.join(FIG, "realised.png"))
plt.close(fig)
print("figures written")
