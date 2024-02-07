# PIBE Database Management Dashboard

The PIBE Database Management Dashboard is a Shiny application designed for managing, querying, and visualizing acoustic and meteorological experimental data from the PIBE database. It provides an interactive and user-friendly interface for researchers and stakeholders involved in the PIBE project.

## Features

- **PIBE Project Presentation**: Explore HTML files presenting details about the PIBE project and the WP2 package.

- **Data Download**: Download data based on specified dates, time sampling, and selected fields, sensors, and indicators.

- **Visualization**: Visualize acoustic and meteorological data through interactive time series plots, scatter plots, and tables.

## Installation

To run the PIBE Database Management Dashboard locally, follow these steps:

1. Clone the repository:

    ```bash
    git clone https://github.com/votre-utilisateur/pibe-dashboard.git
    cd pibe-dashboard
    ```

2. Install required R packages:

    ```R
    # Install necessary packages
    install.packages(c("shiny", "shinydashboard", "leaflet", "plotly", ...))  # Add other required packages
    ```

3. Run the Shiny application:

    ```R
    shiny::runApp()
    ```

## Usage

- Launch the Shiny application by running the `shiny::runApp()` command in R after installing the required packages.

- Use the navigation tabs to explore different sections of the dashboard, including the PIBE project presentation, data download, and visualization.

## Links

- [PIBE Project Website](https://www.anr-pibe.com/en): Visit the official PIBE project website for additional information.


## Author

- **Gwenael Guillaume** - [Contact](mailto:gwenael.guillaume@cerema.fr)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
