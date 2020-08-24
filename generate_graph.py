import matplotlib.pyplot as plt
import csv
import glob 

legend = []
for file in glob.glob("./results/hyperfine/*/*.csv"):
    dates = []
    means = []
    name = file.split("/")[-2]
    with open(file) as csv_file:
        reader = csv.DictReader(csv_file)
        last_date = None
        for row in reader:
            means.append(float(row['mean']))
            if int(row['date']) == last_date:
                dates.append(last_date + 0.1)
            else:
                dates.append(int(row['date']))

            last_date = int(row['date'])
    print(f"Plotting {name} | {dates} {means}")
    # plt.plot(dates, means)
    plt.plot(means)
    legend.append(name)

print("Saving plot")
plt.legend(legend)
#plt.show()
plt.savefig("benchmarks.png")