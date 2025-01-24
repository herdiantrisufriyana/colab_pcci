# Post-COVID Cognitive Impairment among Patients in a Hospital in D.I. Yogyakarta, Indonesia, November 2021 to December 2023

Amelia Nur Vidyanti,1 Muhammad Hardhantyo,2,3 ,† Herdiantri Sufriyana,4,5 Emily Chia-Yu Su,4,5 Hanevi Djasri,2 Astuti Prodjohardjono,1 Rifki Habibi Rahman,6 Bao-Ping Zhu7, Rebecca D. Merrill, 7 Catharina Yekti Praptiningsih, 7 Amalya Mangiri. 7

1 Department of Neurology, Faculty of Medicine, Public Health and Nursing, Universitas Gadjah Mada, Yogyakarta 55281, Indonesia.
2 Center for Health Policy and Management, Faculty of Medicine, Universitas Gadjah Mada, Yogyakarta 55281, Indonesia.
3 Faculty of Health Science, Universitas Respati Yogyakarta, Yogyakarta 55281, Indonesia.
4 Institute of Biomedical Informatics, College of Medicine, National Yang Ming Chiao Tung University, Taipei 112304, Taiwan.
5 Graduate Institute of Biomedical Informatics, College of Medical Science and Technology, Taipei Medical University, Taipei 11031, Taiwan.
6 Neurology Research Office, Department of Neurology, Faculty of Medicine, Public Health and Nursing, Universitas Gadjah Mada, Yogyakarta 55281, Indonesia.
7 U.S. Centers for Disease Control and Prevention.

† Corresponding author:
Muhammad Hardhantyo. Address: Center for Health Policy and Management, Faculty of Medicine, Universitas Gadjah Mada, Yogyakarta 55281, Indonesia. Email address: hardhantyo@gmail.com.

# Overview

Follow instruction below to install the programming environment. Find the source codes in index.Rmd. The simplified output of the source codes is shown  [**here**](https://herdiantrisufriyana.github.io/colab_pcci/index.html).

## System requirements

Install Docker desktop once in your machine. Start the service every time you build this project image or run the container.

## Installation guide

Change `colab_pcci` to the project image name.

Build the project image once for a new machine (currently support AMD64 and ARM64).

```{bash}
docker build -t colab_pcci --load .
```

Run the container every time you start working on the project. Change left-side port numbers for either Rstudio or Jupyter lab if any of them is already used by other applications.

In terminal:

```{bash}
docker run -d -p 8787:8787 -p 8888:8888 -v "$(pwd)":/home/rstudio/project --name colab_pcci_container colab_pcci
```

In command prompt:

```{bash}
docker run -d -p 8787:8787 -p 8888:8888 -v "%cd%":/home/rstudio/project --name colab_pcci_container colab_pcci
```

## Instructions for use

### Rstudio

Change port number in the link, accordingly, if it is already used by other applications.

Visit http://localhost:8787.
Username: rstudio
Password: 1234

Your working directory is ~/project.

### Jupyter lab

Use terminal/command prompt to run the container terminal.

```{bash}
docker exec -it colab_pcci_container bash
```

In the container terminal, run jupyter lab using this line of codes.

```{bash}
jupyter-lab --ip=0.0.0.0 --no-browser --allow-root
```

Click a link in the results to open jupyter lab in a browser. Change port number in the link, accordingly, if it is already used by other applications.






