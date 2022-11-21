# Full ISQ submission (title page + manuscript)
pdftools::pdf_combine(
  c(here::here("04_paper", "isq_title_page.pdf"),
    here::here("04_paper", "isq_manuscript.pdf")),
  here::here("04_paper", "isq_full_manuscript.pdf")
)
