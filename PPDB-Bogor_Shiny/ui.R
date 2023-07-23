library(shiny)
library(shinydashboard)
#library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(reactable)
library(reactablefmtr)
library(leaflet)
library(randomcoloR)
library(shinycssloaders)

# DASHBOARD #
ui <- dashboardPage(
  dashboardHeader(title = "PPDB Kota Bogor (SMP)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prestasi", tabName = "prestasi", icon = icon("trophy")),
      menuItem("Rapor", tabName = "rapor", icon = icon("book")),
      menuItem("Zonasi", tabName = "zonasi", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      ### PRESTASI ###
      tabItem(tabName = "prestasi",
              fluidRow(
                box(
                  width = 4,
                  status = "primary",
                  title = "Sekolah:",
                  selectInput("sekolahInput", "Pilihan Sekolah:",
                              choices = c("Semua Sekolah", unique(smp_prestasi$sekolah_pilihan)))
                ),
                infoBoxOutput("jumlahSiswaPrestasi"),
                infoBoxOutput("prestasiTerbanyak"),
                infoBoxOutput("nilaiTertinggiPrestasi"),
                infoBoxOutput("passingGradePrestasi"),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Prestasi Akademik / Non Akademik",
                  withSpinner(plotlyOutput(
                    outputId = "grafikPrestasi"
                  ))
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Sekolah Teratas",
                  withSpinner(plotlyOutput(
                    outputId = "sekolahPrestasi"
                  ))
                ),
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "Daftar Siswa Hasil Seleksi Prestasi Akademik / Non Akademik",
                  withSpinner(reactableOutput("tabelPrestasi")
                  ))
              )),
      ### RAPOR ###
      tabItem(tabName = "rapor",
              fluidRow(
                box(
                  width = 4,
                  status = "primary",
                  title = "Sekolah:",
                  selectInput("sekolahInputRapor", "Pilihan Sekolah:",
                              choices = c("Semua Sekolah", unique(smp_rapor$sekolah_pilihan)))
                ),
                infoBoxOutput("jumlahSiswaRapor"),
                infoBoxOutput("nilaiTertinggiRapor"),
                infoBoxOutput("passingGradeRapor"),
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Sekolah Teratas",
                  withSpinner(plotlyOutput(
                    outputId = "sekolahRapor"
                  ))
                ),
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "Daftar Siswa Hasil Seleksi Rapor",
                  reactableOutput("tabelRapor")
                )
              )),
      ### ZONASI ###
      tabItem(tabName = "zonasi",
              fluidRow(
                box(
                  width = 6,
                  style = "height: 180px;",
                  status = "primary", solidHeader = TRUE,
                  title = "Sekolah:",
                  selectInput("sekolahInputZonasi", "Pilihan Sekolah:",
                              choices = c("Semua Sekolah", unique(smp_zonasi$pilihan_sekolah)),
                              selected = "SMP NEGERI 1 BOGOR"),
                  selectInput("zonasiInput", "Zonasi:",
                              choices = c("Semua Zona", "Zona 1", "Zona 2", "Zona 3", "Zona 4", "Zona 5", "Zona 6", "Zona 7"))
                ),
                box(width = 6,
                    style = "height: 180px;",
                    status = "primary", solidHeader = TRUE,
                    title = "Status:",
                    selectInput("zonasiStatus", "Status:",
                                choices = c("Semua", unique(smp_zonasi$hasil)),
                                selected = "Diterima"),
                    sliderInput("zonasiUrutan", "Urutan:",
                                min = 1, max = nrow(smp_zonasi), value = 100, step = 1)
                ),                    
                infoBoxOutput("jumlahSiswaZonasi", width = 2),
                infoBoxOutput("jumlahTerimaZonasi", width = 2),
                infoBoxOutput("jumlahTolakZonasi", width = 2),
                infoBoxOutput("jarakTerdekatZonasi", width = 2),
                infoBoxOutput("jarakTerjauhZonasi", width = 2),
                infoBoxOutput("sekolahTeratasZonasi", width = 2),
                box(
                  width = 8,
                  status = "success", solidHeader = TRUE,
                  title = "Peta Penerimaan Jalur Zonasi",
                  withSpinner(
                    leafletOutput("petaZonasi")
                  )
                ),
                box(
                  width = 4,
                  status = "info", solidHeader = TRUE,
                  title = "Persentase Zona",
                  withSpinner(plotlyOutput(
                    outputId = "persentaseZona"
                  ))
                ),
                box(
                  width = 8,
                  status = "info", solidHeader = TRUE,
                  title = "Sekolah Teratas per Zona",
                  withSpinner(plotlyOutput(
                    outputId = "topSekolahZonasiZona"
                  ))
                ),
                box(
                  width = 4,
                  status = "success", solidHeader = TRUE,
                  title = "Sekolah Teratas per Status Penerimaan",
                  withSpinner(plotlyOutput(
                    outputId = "topSekolahZonasiStatus"
                  ))
                ),
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "Daftar Siswa Hasil Seleksi Zonasi",
                  withSpinner(reactableOutput("tabelZonasi"))
                )
              ))
    )
  ),
  skin = "green"
)

