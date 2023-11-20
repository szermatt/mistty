# Configuration file for the Sphinx documentation builder.

# -- Project information

project = 'MisTTY'
copyright = '2023, Stephane Zermatten'
author = 'Stephane Zermatten'

release = '1.0.9'
version = '1.0.9'

# -- General configuration

extensions = [
    'sphinx.ext.duration',
    'sphinx.ext.doctest',
    'sphinx.ext.autodoc',
    'sphinx.ext.autosummary',
    'sphinx.ext.intersphinx',
]

root_doc = "index"

intersphinx_mapping = {
    'python': ('https://docs.python.org/3/', None),
    'sphinx': ('https://www.sphinx-doc.org/en/master/', None),
}
intersphinx_disabled_domains = ['std']

templates_path = ['_templates']

# -- Options for HTML output

html_theme = 'sphinx_rtd_theme'

# -- Options for EPUB output
epub_show_urls = 'footnote'

# -- Options for Texinfo output

texinfo_documents = [
    (
        # startdocname
        root_doc,
        # targetname
        "mistty",
        # title
        "MisTTY",
        # author
        "Stephane Zermatten",
        # dir_entry
        "MisTTY",
        # description
        "Shell/comint alternative with a fully-functional terminal",
        # category
        "Emacs",
        # toctree_only
        False,
    )
]
