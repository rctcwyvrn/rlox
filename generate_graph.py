import matplotlib.pyplot as plt
import csv
import glob 
import numpy as np

legend = []
all_gains = []
for file in glob.glob("./results/hyperfine/*/*.csv"):
    dates = []
    means = []
    gains = []
    name = file.split("/")[-2]
    with open(file) as csv_file:
        reader = csv.DictReader(csv_file)
        last_date = None
        last_mean = None
        for row in reader:
            means.append(float(row['mean']))

            if last_mean != None:
                percent_gain = 100 * (last_mean - float(row['mean'])) / last_mean
                gains.append(percent_gain)
            
            if int(row['date']) == last_date:
                dates.append(last_date + 0.1)
            else:
                dates.append(int(row['date']))

            last_date = int(row['date'])
            last_mean = float(row['mean'])
    print(f"Plotting {name} | {dates} {means}")
    # plt.plot(dates, means)
    plt.plot(means)
    legend.append(name)
    all_gains.append([name, gains])

print("Saving plot")
plt.legend(legend)
#plt.show()
plt.savefig("benchmarks.png")
plt.clf()

plt.figure(figsize=(10,50))
ind = np.arange(len(dates)-1, step=1)
plots = []
names = []
for i, (name, gains) in enumerate(all_gains):
    print(f"Plotting gains {name} | {gains}")
    #p = plt.bar(ind, gains)

    plt.subplot(10, 1, i+1)
    plt.title(name)
    plt.ylim(-50,50)
    plt.xticks(ind, dates)
    p = plt.bar(ind, gains, width=0.1)
    plots.append(p[0])

plt.savefig("gains.png")