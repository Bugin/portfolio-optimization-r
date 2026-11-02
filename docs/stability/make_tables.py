"""LaTeX tables of the stability study, from ./out to ./tables.tex."""
import os
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
import sys
TAG = sys.argv[1] if len(sys.argv) > 1 else "10k"
OUT = os.path.join(HERE, f"out_{TAG}")
m = pd.read_csv(os.path.join(OUT, "monthly_runs.csv"))
noise = pd.read_csv(os.path.join(OUT, "noise_seeds.csv"))
cross = pd.read_csv(os.path.join(OUT, "cross_eval.csv"))
added = pd.read_csv(os.path.join(OUT, "month_added.csv"))
corr = pd.read_csv(os.path.join(OUT, "correlations.csv"))
months = list(m.month.unique())
A = ["Euro Govt", "Euro Corp IG", "Euro Corp HY", "Europe Equity"]
SHORT = {"Euro Govt": "Govt", "Euro Corp IG": "IG", "Euro Corp HY": "HY", "Europe Equity": "Equity"}
FL = {"daily": "giornaliero", "weekly": "settimanale"}
parts = []


NSC = {"10k": r"10\,000", "100k": r"100\,000"}.get(TAG, TAG)
LAB = "" if TAG == "100k" else "_" + TAG

def table(caption, label, header, rows, align, size="small"):
    caption = caption.rstrip() + f" Run con {NSC} scenari."
    label = label + LAB
    s = [f"\\begin{{table}}[htbp]\\centering\\{size}\\setlength{{\\tabcolsep}}{{3.5pt}}",
         f"\\caption{{{caption}}}\\label{{{label}}}",
         f"\\begin{{tabular}}{{{align}}}\\toprule", header + " \\\\\\midrule"]
    s += [r + " \\\\" for r in rows]
    s += ["\\bottomrule\\end{tabular}\\end{table}"]
    parts.append("\n".join(s))


# --- summary: range over the 13 months vs range over seeds -------------------
rows = []
items = [("minES_es", "Min-ES: ES 95\\,\\%", 100, "pp"), ("minES_ret", "Min-ES: E[R]", 100, "pp"),
         ("maxR_ratio", "Max-ratio: rapporto", 1, ""), ("maxR_es", "Max-ratio: ES 95\\,\\%", 100, "pp"),
         ("maxR_ret", "Max-ratio: E[R]", 100, "pp")]
for f in ["daily", "weekly"]:
    d = m[m.freq == f]
    n_last = noise[(noise.month == months[-1]) & (noise.freq == f)]
    n_first = noise[(noise.month == months[0]) & (noise.freq == f)]
    rows.append(f"\\multicolumn{{6}}{{l}}{{\\emph{{Rendimenti {FL[f]}}}}}")
    for col, lab, sc, unit in items:
        fmt = (lambda v: f"{sc * v:.2f}") if sc == 100 else (lambda v: f"{v:.3f}")
        rows.append(f"{lab} & {fmt(d[col].min())} & {fmt(d[col].max())} & {fmt(d[col].max() - d[col].min())}"
                    f" & {fmt(n_first[col].max() - n_first[col].min())} & {fmt(n_last[col].max() - n_last[col].min())}")
    for tag, lab in [("minES", "Min-ES"), ("maxR", "Max-ratio")]:
        for a in A:
            col = f"{tag}_{a}"
            rows.append(f"{lab}: peso {SHORT[a]} & {100 * d[col].min():.1f} & {100 * d[col].max():.1f} & {100 * (d[col].max() - d[col].min()):.1f}"
                        f" & {100 * (n_first[col].max() - n_first[col].min()):.1f} & {100 * (n_last[col].max() - n_last[col].min()):.1f}")
table("Escursione delle metriche e dei pesi nei 13 run mensili, a confronto con l'escursione dovuta al solo seed Monte Carlo "
      "(6 seed, stessi dati) alla prima (ott 2025) e all'ultima data (ott 2026). Metriche in \\%, pesi in punti percentuali.",
      "tab:summary", "grandezza & min & max & escursione & seed ott 2025 & seed ott 2026", rows, "lrrrrr")

# --- full monthly results ----------------------------------------------------
for f in ["daily", "weekly"]:
    d = m[m.freq == f]
    rows = []
    for _, r in d.iterrows():
        w1 = " & ".join(f"{100 * r[f'minES_{a}']:.1f}" for a in A)
        w2 = " & ".join(f"{100 * r[f'maxR_{a}']:.1f}" for a in A)
        rows.append(f"{r.month} & {int(r.n_periods)} & {100 * r.minES_ret:.2f} & {100 * r.minES_es:.2f} & {w1}"
                    f" & {100 * r.maxR_ret:.2f} & {100 * r.maxR_es:.2f} & {r.maxR_ratio:.3f} & {w2}")
    table(f"Run mensili con rendimenti {FL[f]}, seed 42. $n$ = periodi nel campione; E[R] ed ES in \%, pesi in \%.",
          f"tab:runs_{f}",
          "mese & $n$ & \multicolumn{6}{c}{Min-ES: E[R], ES, pesi Govt/IG/HY/Eq} & \multicolumn{7}{c}{Max-ratio: E[R], ES, rapporto, pesi Govt/IG/HY/Eq}",
          rows, "lrrrrrrrrrrrrrr", size="scriptsize")

# --- seed noise at the last date ---------------------------------------------
rows = []
for f in ["daily", "weekly"]:
    d = noise[(noise.month == months[-1]) & (noise.freq == f)]
    for _, r in d.iterrows():
        w1 = "/".join(f"{100 * r[f'minES_{a}']:.1f}" for a in A)
        w2 = "/".join(f"{100 * r[f'maxR_{a}']:.1f}" for a in A)
        rows.append(f"{FL[f]} & {int(r.seed)} & {100 * r.minES_es:.2f} & {w1} & {r.maxR_ratio:.3f} & {100 * r.maxR_es:.2f} & {w2}")
table("Stessi dati (fino al 30 ott 2026), sei seed del bootstrap: la variabilit\\`a dovuta al solo campionamento Monte Carlo.",
      "tab:seeds", "freq. & seed & Min-ES: ES & pesi & Max-ratio: rapporto & ES & pesi", rows, "lrrlrrl", size="footnotesize")

# --- cross evaluation --------------------------------------------------------
rows = []
for f in ["daily", "weekly"]:
    d = cross[cross.freq == f]
    for _, r in d.iterrows():
        if pd.isna(r.minES_es_on_next):
            nx = "-- & --"
        else:
            nx = f"{100 * (r.minES_es_on_next - r.minES_es_next_opt):+.3f} & {r.maxR_ratio_on_next - r.maxR_ratio_next_opt:+.4f}"
        rows.append(f"{FL[f]} & {r.month} & {nx} & {100 * (r.minES_es_on_final - r.minES_es_final_opt):+.3f}"
                    f" & {r.maxR_ratio_on_final - r.maxR_ratio_final_opt:+.4f}")
table("Costo di usare i pesi di un mese sugli scenari del mese successivo e sugli scenari finali (ott 2026): "
      "differenza fra la metrica dei pesi ``vecchi'' e l'ottimo di quel mese. ES in punti percentuali, rapporto in valore assoluto.",
      "tab:cross", "freq. & pesi del mese & $\\Delta$ES, mese dopo & $\\Delta$rapp., mese dopo & $\\Delta$ES, finale & $\\Delta$rapp., finale",
      rows, "llrrrr", size="footnotesize")

# --- what each month added ----------------------------------------------------
rows = []
for _, r in added.iterrows():
    if r.month == months[0]:
        continue
    rets = " & ".join(f"{100 * r[f'ret_{a}']:+.1f}" for a in A)
    worst = " & ".join(f"{100 * r[f'worst_{a}']:+.1f}" for a in A)
    tails = "/".join(str(int(r[f"ntail_{a}"])) for a in A)
    rows.append(f"{r.month} & {int(r.n_days)} & {rets} & {worst} & {tails}")
table("Cosa ha aggiunto ogni mese al campione: rendimento del mese per asset (\\%), peggior giorno (\\%) e numero di giorni "
      "sotto il 5\\textsuperscript{o} percentile storico dei rendimenti giornalieri (Govt/IG/HY/Equity).",
      "tab:added", "mese & giorni & \\multicolumn{4}{c}{rendimento: Govt, IG, HY, Eq} & \\multicolumn{4}{c}{peggior giorno} & giorni in coda",
      rows, "lrrrrrrrrrl", size="scriptsize")

# --- correlations and dispersion daily vs weekly -----------------------------
rows = []
for _, r in corr.iterrows():
    rows.append(f"{FL[r.freq]} & {r.month} & {int(r.n)} & {r.cor_govt_ig:.2f} & {r.cor_govt_hy:.2f} & {r.cor_govt_eq:.2f}"
                f" & {r.cor_ig_hy:.2f} & {r.cor_ig_eq:.2f} & {r.cor_hy_eq:.2f}")
table("Correlazioni dei log-rendimenti a frequenza giornaliera e settimanale, alla prima e all'ultima data.",
      "tab:corr", "freq. & data & $n$ & Govt--IG & Govt--HY & Govt--Eq & IG--HY & IG--Eq & HY--Eq", rows, "llrrrrrrr")

rows = []
for f in ["daily", "weekly"]:
    r = m[(m.freq == f) & (m.month == months[-1])].iloc[0]
    rows.append(f"{FL[f]} & " + " & ".join(f"{100 * r[f'mean_{a}']:.2f}" for a in A)
                + " & " + " & ".join(f"{100 * r[f'std_{a}']:.2f}" for a in A)
                + " & " + " & ".join(f"{100 * r[f'es_{a}']:.2f}" for a in A))
table("Statistiche dei singoli asset all'orizzonte (60 giorni, scenari del run di ott 2026, seed 42), in \\%.",
      "tab:assets", "freq. & \\multicolumn{4}{c}{E[R]: Govt, IG, HY, Eq} & \\multicolumn{4}{c}{dev. std} & \\multicolumn{4}{c}{ES 95\\,\\%}",
      rows, "lrrrrrrrrrrrr")

with open(os.path.join(HERE, f"tables_{TAG}.tex"), "w", encoding="utf8") as fh:
    fh.write("\n\n".join(parts) + "\n")
with open(os.path.join(HERE, f"tables_{TAG}_summary.tex"), "w", encoding="utf8") as fh:
    fh.write(parts[0] + "\n")
with open(os.path.join(HERE, f"tables_{TAG}_detail.tex"), "w", encoding="utf8") as fh:
    fh.write("\n\n".join(parts[1:]) + "\n")
print("tables written")


# --- extra tables when the 10k run is also available -------------------------
noise10 = os.path.join(HERE, "out_10k", "noise_seeds.csv")
bench = os.path.join(HERE, "benchmark.csv")
extra = []
if TAG != "10k" and os.path.exists(noise10):
    n10 = pd.read_csv(noise10)
    rows = []
    cols = [("minES_es", r"Min-ES: ES 95\,\%", 100), ("minES_ret", "Min-ES: E[R]", 100),
            ("maxR_ratio", "Max-ratio: rapporto", 1), ("maxR_es", r"Max-ratio: ES 95\,\%", 100)]
    cols += [(f"minES_{a}", f"Min-ES: peso {SHORT[a]}", 100) for a in A]
    cols += [(f"maxR_{a}", f"Max-ratio: peso {SHORT[a]}", 100) for a in A]
    for col, lab, sc in cols:
        cells = []
        for f in ["daily", "weekly"]:
            for src in [n10, noise]:
                g = src[(src.month == months[-1]) & (src.freq == f)][col]
                r = sc * (g.max() - g.min())
                cells.append(f"{r:.3f}" if sc == 1 else (f"{r:.2f}" if "peso" not in lab else f"{r:.1f}"))
        rows.append(f"{lab} & " + " & ".join(cells))
    extra.append("\n".join([
        r"\begin{table}[htbp]\centering\small\setlength{\tabcolsep}{3.5pt}",
        r"\caption{Rumore Monte Carlo: escursione su 6 seed a dati fissi (30 ott 2026) con 10\,000 e con 100\,000 scenari. "
        r"Metriche in \%, pesi in punti percentuali.}\label{tab:noise_cmp}",
        r"\begin{tabular}{lrrrr}\toprule",
        r"grandezza & \multicolumn{2}{c}{giornaliero} & \multicolumn{2}{c}{settimanale} \\",
        r" & 10k & 100k & 10k & 100k \\ \midrule"] + [r + r" \\" for r in rows] +
        [r"\bottomrule\end{tabular}\end{table}"]))
if os.path.exists(bench):
    bd = pd.read_csv(bench).groupby("n_scenarios").min(numeric_only=True)
    rows = []
    for n, r in bd.iterrows():
        rows.append(f"{int(n):,}".replace(",", r"\,") + f" & {r.t_simulate:.2f} & {r.t_min_es:.1f} & {r.t_max_ratio:.1f} & {r.t_frontier_point:.1f}"
                    f" & {r.t_simulate + r.t_min_es + r.t_max_ratio + 20 * r.t_frontier_point:.0f} & {r.mem_scen_mb:.1f}")
    extra.append("\n".join([
        r"\begin{table}[htbp]\centering\footnotesize\setlength{\tabcolsep}{4pt}",
        r"\caption{Tempi di calcolo (secondi, minimo di due ripetizioni) al variare del numero di scenari; dati a ott 2026, "
        r"rendimenti giornalieri. LP del notebook = simulazione + due LP + 20 punti di frontiera, senza nuvola casuale e grafici.}\label{tab:bench}",
        r"\begin{tabular}{rrrrrrr}\toprule",
        r"scenari & simulazione & LP Min-ES & LP Max-ratio & punto frontiera & LP del notebook & scenari (MB) \\ \midrule"]
        + [r + r" \\" for r in rows] + [r"\bottomrule\end{tabular}\end{table}"]))
with open(os.path.join(HERE, f"tables_{TAG}_extra.tex"), "w", encoding="utf8") as fh:
    fh.write("\n\n".join(extra) + "\n")
print("extra tables written")
