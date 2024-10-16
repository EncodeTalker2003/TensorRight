from typing import Sequence
import dataclasses
import numpy as np
import matplotlib.pyplot as plt

@dataclasses.dataclass
class Result:
    theory: str
    num_of_tasks: int
    has_warning: bool
    time: float
    succeeded: bool


@dataclasses.dataclass
class Rule:
    name: str
    results: list[Result]
    succeeeded: bool
    def all_same_num_of_tasks(self) -> bool:
        return all(x.num_of_tasks == self.results[0].num_of_tasks for x in self.results)
    def first_num_of_tasks(self) -> int:
        return self.results[0].num_of_tasks
    def overall_time(self) -> float:
        return sum(x.time for x in self.results)


def parse_file(lines: Sequence[str]) -> list[Rule]:
    """
    The file looks like this:
    ====> Slice(Concat(A,B), A.size, A.size+B.size) ⇒ B
    >>> Bool
    [INFO-Bool]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
    [INFO-Bool]: Number of bounded verification tasks: 1
    [SUCCESS-Bool]: [0.12387501999910455s] Verification succeeded.
    >>> Int
    [INFO-Int]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
    [INFO-Int]: Number of bounded verification tasks: 1
    [SUCCESS-Int]: [0.11165066000830848s] Verification succeeded.
    >>> Real
    [INFO-Real]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
    [INFO-Real]: Number of bounded verification tasks: 1
    [SUCCESS-Real]: [0.106301934007206s] Verification succeeded.
    >>> Overall
    [SUCCESS-Overall]: [0.34182761401461903s] Verification succeeded.
    ====> Slice(Broadcast(A)) ⇒ Broadcast(A)
    >>> Bool
    [INFO-Bool]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
    [INFO-Bool]: Number of bounded verification tasks: 1
    [SUCCESS-Bool]: [0.34009405800316017s] Verification succeeded.
    >>> Int
    [INFO-Int]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
    [INFO-Int]: Number of bounded verification tasks: 1
    [SUCCESS-Int]: [0.23800732199742924s] Verification succeeded.
    >>> Real
    [INFO-Real]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
    [INFO-Real]: Number of bounded verification tasks: 1
    [SUCCESS-Real]: [0.26108629700320307s] Verification succeeded.
    >>> Overall
    [SUCCESS-Overall]: [0.8391876770037925s] Verification succeeded.
    """

    rules: list[Rule] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.startswith("====>"):
            name = " ".join(line.split(" ")[1:])
            results: list[Result] = []
            has_warning = False
            i += 1
            while i < len(lines) and not lines[i].startswith("====>"):
                if lines[i].startswith(">>>"):
                    theory = lines[i].split(" ")[1].strip()
                    if theory != "Overall":
                        i += 1
                        if lines[i].startswith("[FAIL"):
                            time = float(lines[i].split(" ")[1][1:-2])
                            results.append(Result(theory, -1, False, time, False))
                            i += 1
                            continue
                        i += 1
                        num_of_tasks = int(lines[i].split(" ")[-1])
                        i += 1
                        while lines[i].startswith("[WARNING"):
                            has_warning = True
                            i += 1
                        time = float(lines[i].split(" ")[1][1:-2])
                        succeeded = lines[i].startswith("[SUCCESS")
                        results.append(
                            Result(theory, num_of_tasks, has_warning, time, succeeded))
                    else:
                        i += 1
                        time = float(lines[i].split(" ")[1][1:-2])
                        succeeded = lines[i].startswith("[SUCCESS")
                        rules.append(Rule(name, results, succeeded))
                        i += 1
                        break
                else:
                    # No theory is given
                    i += 1
                    num_of_tasks = int(lines[i].split(" ")[-1])
                    i += 1
                    while lines[i].startswith("[WARNING"):
                        has_warning = True
                        i += 1
                    time = float(lines[i].split(" ")[1][1:-2])
                    succeeded = lines[i].startswith("[SUCCESS")
                    results.append(Result("", num_of_tasks, has_warning, time, succeeded))
                    rules.append(Rule(name, results, succeeded))
                    i += 1
                    break
                i += 1
        else:
            i += 1
    return rules


sample = """====> Slice(Concat(A,B), A.size, A.size+B.size) ⇒ B
>>> Bool
[INFO-Bool]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
[INFO-Bool]: Number of bounded verification tasks: 1
[SUCCESS-Bool]: [0.12387501999910455s] Verification succeeded.
>>> Int
[INFO-Int]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
[INFO-Int]: Number of bounded verification tasks: 1
[SUCCESS-Int]: [0.11165066000830848s] Verification succeeded.
>>> Real
[INFO-Real]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
[INFO-Real]: Number of bounded verification tasks: 1
[SUCCESS-Real]: [0.106301934007206s] Verification succeeded.
>>> Overall
[SUCCESS-Overall]: [0.34182761401461903s] Verification succeeded.
====> Slice(Broadcast(A)) ⇒ Broadcast(A)
>>> Bool
[INFO-Bool]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
[INFO-Bool]: Number of bounded verification tasks: 1
[SUCCESS-Bool]: [0.34009405800316017s] Verification succeeded.
>>> Int
[INFO-Int]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
[INFO-Int]: Number of bounded verification tasks: 1
[SUCCESS-Int]: [0.23800732199742924s] Verification succeeded.
>>> Real
[INFO-Real]: Inferred bounds: fromList [(adim1,1),(adim0,1)]
[INFO-Real]: Number of bounded verification tasks: 1
[SUCCESS-Real]: [0.26108629700320307s] Verification succeeded.
>>> Overall
[SUCCESS-Overall]: [0.8391876770037925s] Verification succeeded.
""".splitlines()
# print(parse_file(sample))
with open("result.txt", "r") as f:
    res = parse_file(list(map(lambda x: x.strip(), f.readlines())))

number_of_failed_inference = len(list(filter(lambda x: x < 0, map(lambda x: x.first_num_of_tasks(), res))))
number_of_tasks = list(filter(lambda x: x >= 0, map(lambda x: x.first_num_of_tasks(), res)))

times = list(map(lambda x: x.overall_time(), res))

bins = np.logspace(np.log10(0.001),np.log10(max(t for t in times)), 5)

plt.hist(times, bins=20, edgecolor='black')

plt.title('Total Time taken for Unbounded Verification')
plt.xlabel('Time (seconds)')
plt.ylabel('Number of Rules')

plt.savefig('timing_plot.pdf', format="pdf", bbox_inches="tight", dpi=600)
