from setuptools import setup, find_packages

with open('README.md', 'r') as f:
    long_description = f.read()

setup(
    name='hyuga',
    version='0.0.1',
    author='Yuuta Sakurai',
    author_email='sakurai.yuta@gmail.com',
    description='Hyuga - Yet another hy language server',
    long_description=long_description,
    long_description_content_type='text/markdown',
    url='https://github.com/sakuraiyuta/hyuga',
    license="MIT",
    packages=['hyuga'],
    package_data={
        'hyuga': ['*.hy', '__pycache__/*']
    },
    python_requires='>=3.6',
    install_requires=[
        'hy',
        'hyrule',
        'toolz',
        'pygls',
    ],
    entry_points={
        'console_scripts': [
            'hyuga=hyuga.__main__:main'
        ]
    }
)
