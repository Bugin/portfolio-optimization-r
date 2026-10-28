"""Export the cross-check fixtures for the R translation from the Python notebook.

Run from the Python repo folder (https://github.com/Bugin/portfolio-optimization):

    python <path>/export_from_python.py

It executes the code cells of `src/es_cells.py` that precede the `## Run`
section (same loader as `tests/conftest.py`), replicates the Run cell up to the
scenarios, and writes next to this script:

- `python_scenarios.csv`: the 10 000 x 4 bootstrap scenarios (seed 42);
- `python_expected.json`: min-ES, max-ratio and the mean-ES frontier on those
  scenarios, for trivial bounds and for a tight bound set.
"""
import json
from pathlib import Path

import matplotlib
import numpy as np

matplotlib.use('Agg')

CELLS = Path('src') / 'es_cells.py'
RUN_MARKER = '# %% [markdown]\n# ## Run'
OUT = Path(__file__).resolve().parent


def load_namespace():
    """Namespace after executing every code cell that precedes the Run section."""
    text = CELLS.read_text(encoding='utf-8')
    cut = text.find(RUN_MARKER)
    if cut != -1:
        text = text[:cut]
    chunks, current = [], []
    for line in text.splitlines(keepends=True):
        if line.startswith('# %%'):
            chunks.append(current)
            current = []
        current.append(line)
    chunks.append(current)
    namespace = {'__name__': 'es_cells'}
    for chunk in chunks:
        if chunk and chunk[0].startswith('# %%') and '[markdown]' not in chunk[0]:
            exec(''.join(chunk), namespace)
    return namespace


def portfolio_dict(p):
    return {'w': p['w'].tolist(), 'ret': float(p['ret']), 'es': float(p['es']),
            'ratio': float(p['ratio'])}


def main():
    ns = load_namespace()
    prices, logret = ns['get_prices_and_logret'](ns['TICKERS'])
    logret = logret.rename(columns=ns['NAMES']).rename_axis(columns=None)
    names = list(logret.columns)
    level, n_frontier = ns['ES_LEVEL'], ns['N_FRONTIER']
    rf_h = float(np.exp(ns['RISK_FREE'] * ns['HORIZON_DAYS'] / 252) - 1)
    scenarios = ns['simulate_horizon_returns'](logret, ns['HORIZON_DAYS'], ns['N_SCENARIOS'],
                                               ns['SIM_RNG'])
    scenarios.to_csv(OUT / 'python_scenarios.csv', index=False, float_format='%.12g')

    bound_sets = {
        'trivial': ns['BOUNDS'],
        'tight': {'Europe Equity': (0.0, 0.2), 'Euro Govt': (0.1, 1.0)},
    }
    out = {'level': float(level), 'rf_h': rf_h, 'n_frontier': int(n_frontier), 'assets': names,
           'sample': {'first': str(prices.index[0].date()), 'last': str(prices.index[-1].date()),
                      'n_days': int(len(logret))},
           'bounds': {}}
    for label, b in bound_sets.items():
        lo, hi = ns['check_bounds'](b, names)
        min_es = ns['min_es_portfolio'](scenarios, level, lo, hi, rf_h)
        max_ratio = ns['max_ratio_portfolio'](scenarios, level, lo, hi, rf_h)
        r_lo = float(min_es['ret'])
        r_hi = float(0.999 * ns['max_return'](scenarios, lo, hi))
        f_es, f_ret, _ = ns['build_es_frontier'](scenarios, level, lo, hi, rf_h, n_frontier, r_lo, r_hi)
        out['bounds'][label] = {
            'lo': lo.tolist(), 'hi': hi.tolist(),
            'min_es': portfolio_dict(min_es), 'max_ratio': portfolio_dict(max_ratio),
            'r_lo': r_lo, 'r_hi': r_hi,
            'frontier_es': f_es.tolist(), 'frontier_ret': f_ret.tolist(),
        }
        print(f"{label}: min-ES ES={min_es['es']:.6f} ret={min_es['ret']:.6f} | "
              f"max-ratio ratio={max_ratio['ratio']:.6f} | frontier {len(f_es)} points")
    (OUT / 'python_expected.json').write_text(json.dumps(out, indent=2))
    print(f"Sample {out['sample']}; wrote {OUT / 'python_scenarios.csv'} and python_expected.json")


if __name__ == '__main__':
    main()
