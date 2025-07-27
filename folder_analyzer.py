#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Анализатор папки ABAP файлов с использованием Claude4Parser
Автор: AI Assistant
Версия: 1.0
"""

import sys
import time
import json
import importlib.util
import os
from typing import Dict, Any, List, Set, Optional
from tabulate import tabulate
from pathlib import Path
from io import StringIO


class FolderAnalyzer:
    """Анализатор папки ABAP файлов с использованием Claude4Parser"""
    
    def __init__(self, folder_path: str):
        self.folder_path = folder_path
        self.file_results = {}  # Результаты для каждого файла
        self.summary_stats = {}  # Сводная статистика
        self.markdown_content = []  # Содержимое для markdown файла
        print("Загрузка базовых файлов...")
        self.base_objects = self._load_base_objects()
        print(f"Загрузка завершена. Всего объектов в базе: {sum(len(objects) for objects in self.base_objects.values())}\n")
        
    def _load_base_objects(self) -> Dict[str, Set[str]]:
        """Загружает базовые объекты из файлов в папке Base"""
        # Ищем папку Base относительно скрипта
        script_dir = os.path.dirname(os.path.abspath(__file__))
        base_dir = os.path.join(script_dir, 'Base')
        base_objects = {
            'classes_and_interfaces': set(),
            'functions': set(),
            'database_tables': set()
        }
        
        # Маппинг файлов к категориям
        file_mapping = {
            'classes.txt': 'classes_and_interfaces',
            'functions.txt': 'functions',
            'tables.txt': 'database_tables'
        }
        
        for filename, category in file_mapping.items():
            file_path = os.path.join(base_dir, filename)
            if os.path.exists(file_path):
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        for line in f:
                            line = line.strip()
                            # Пропускаем пустые строки, комментарии и заголовки
                            if line and not line.startswith('#') and not line.startswith('Имя'):
                                base_objects[category].add(line)
                    print(f"Загружено {len(base_objects[category])} объектов из {filename}")
                except Exception as e:
                    print(f"Ошибка при загрузке {filename}: {e}")
            else:
                print(f"Файл {filename} не найден в папке Base")
                
        return base_objects
    
    def _check_object_existence(self, item: str, category: str) -> bool:
        """Проверяет существование объекта в базовых файлах"""
        if category == 'other_ddic_dependencies':
            return None  # Не проверяем базу для OTHER DDIC DEPENDENCIES
        return item in self.base_objects.get(category, set())
    
    def _load_parser_module(self):
        """Динамическая загрузка модуля Claude4Parser"""
        parser_path = "abap_objects_parser.py"
        spec = importlib.util.spec_from_file_location("claude4_parser", parser_path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    
    def _get_abap_files(self) -> List[str]:
        """Получает список всех ABAP файлов в папке"""
        abap_files = []
        folder_path = Path(self.folder_path)
        
        if not folder_path.exists():
            raise FileNotFoundError(f"Папка не найдена: {self.folder_path}")
            
        if not folder_path.is_dir():
            raise NotADirectoryError(f"Указанный путь не является папкой: {self.folder_path}")
        
        # Ищем файлы с расширениями .abap, .txt (могут содержать ABAP код)
        for file_path in folder_path.rglob('*'):
            if file_path.is_file() and file_path.suffix.lower() in ['.abap', '.txt']:
                abap_files.append(str(file_path))
                
        return sorted(abap_files)
    
    def _parse_single_file(self, file_path: str) -> Dict[str, Any]:
        """Парсит один ABAP файл с помощью Claude4Parser"""
        start_time = time.time()
        
        try:
            # Загружаем модуль Claude4Parser
            claude4_module = self._load_parser_module()
            
            # Создаем экземпляр парсера и запускаем
            parser = claude4_module.ABAPParser()
            result = parser.parse_file(file_path)
            
            return {
                'result': result,
                'execution_time': time.time() - start_time,
                'status': 'success',
                'file_size': os.path.getsize(file_path)
            }
            
        except Exception as e:
            return {
                'result': None,
                'execution_time': time.time() - start_time,
                'status': 'error',
                'error': str(e),
                'file_size': os.path.getsize(file_path) if os.path.exists(file_path) else 0
            }
    
    def _analyze_file_results(self, file_path: str, parse_result: Dict[str, Any]) -> Dict[str, Any]:
        """Анализирует результаты парсинга файла и сравнивает с базой"""
        if parse_result['status'] != 'success' or not parse_result['result']:
            return {
                'categories': {},
                'total_objects': 0,
                'base_match_stats': {},
                'overall_base_match': 0
            }
        
        result = parse_result['result']
        categories = ['classes_and_interfaces', 'functions', 'database_tables', 'other_ddic_dependencies']
        
        analysis = {
            'categories': {},
            'total_objects': 0,
            'base_match_stats': {},
            'overall_base_match': 0
        }
        
        total_checkable_items = 0
        total_existing_items = 0
        
        for category in categories:
            items = set(result.get(category, []))
            analysis['categories'][category] = {
                'items': sorted(items),
                'count': len(items)
            }
            analysis['total_objects'] += len(items)
            
            # Проверяем соответствие базе (кроме other_ddic_dependencies)
            if category != 'other_ddic_dependencies':
                existing_items = sum(1 for item in items if self._check_object_existence(item, category))
                total_items = len(items)
                
                if total_items > 0:
                    percentage = (existing_items / total_items) * 100
                    analysis['base_match_stats'][category] = {
                        'existing': existing_items,
                        'total': total_items,
                        'percentage': percentage
                    }
                    
                    total_checkable_items += total_items
                    total_existing_items += existing_items
                else:
                    analysis['base_match_stats'][category] = {
                        'existing': 0,
                        'total': 0,
                        'percentage': 0
                    }
        
        # Общий процент соответствия базе
        if total_checkable_items > 0:
            analysis['overall_base_match'] = (total_existing_items / total_checkable_items) * 100
        
        return analysis
    
    def analyze_folder(self):
        """Анализирует все ABAP файлы в папке"""
        print(f"Поиск ABAP файлов в папке: {self.folder_path}")
        abap_files = self._get_abap_files()
        
        if not abap_files:
            print("ABAP файлы не найдены в указанной папке")
            return
            
        print(f"Найдено {len(abap_files)} ABAP файлов")
        print("=" * 80)
        
        # Анализируем каждый файл
        for i, file_path in enumerate(abap_files, 1):
            relative_path = os.path.relpath(file_path, self.folder_path)
            print(f"[{i}/{len(abap_files)}] Анализ файла: {relative_path}")
            
            # Парсим файл
            parse_result = self._parse_single_file(file_path)
            
            # Анализируем результаты
            analysis = self._analyze_file_results(file_path, parse_result)
            
            # Сохраняем результаты
            self.file_results[relative_path] = {
                'parse_result': parse_result,
                'analysis': analysis,
                'absolute_path': file_path
            }
            
            # Выводим краткую информацию
            if parse_result['status'] == 'success':
                print(f"  ✓ Найдено объектов: {analysis['total_objects']}, "
                      f"Соответствие базе: {analysis['overall_base_match']:.1f}%, "
                      f"Время: {parse_result['execution_time']:.3f}с")
            else:
                print(f"  ✗ Ошибка: {parse_result.get('error', 'Неизвестная ошибка')}")
        
        print("\n" + "=" * 80)
        print("Анализ завершен")
    
    def generate_summary_table(self, to_markdown: bool = False):
        """Генерирует сводную таблицу результатов"""
        title = "СВОДНАЯ ТАБЛИЦА РЕЗУЛЬТАТОВ"
        separator = "=" * 80
        
        if to_markdown:
            self.markdown_content.append(f"\n## {title}\n")
        else:
            print("\n" + separator)
            print(title)
            print(separator)
        
        if not self.file_results:
            message = "Нет результатов для отображения"
            if to_markdown:
                self.markdown_content.append(f"*{message}*\n")
            else:
                print(message)
            return
        
        # Подготавливаем данные для таблицы
        table_data = []
        categories = ['classes_and_interfaces', 'functions', 'database_tables', 'other_ddic_dependencies']
        category_names = ['Классы/Интерфейсы', 'Функции', 'Таблицы БД', 'Прочие DDIC']
        
        for file_path, data in self.file_results.items():
            if data['parse_result']['status'] == 'success':
                analysis = data['analysis']
                row = [file_path]
                
                # Добавляем количество объектов по категориям
                for category in categories:
                    count = analysis['categories'].get(category, {}).get('count', 0)
                    row.append(count)
                
                # Добавляем общее количество и процент соответствия базе
                row.append(analysis['total_objects'])
                row.append(f"{analysis['overall_base_match']:.1f}%")
                row.append(f"{data['parse_result']['execution_time']:.3f}с")
                
            else:
                # Файл с ошибкой
                row = [f"{file_path} ❌"]
                row.extend(['-'] * (len(categories) + 3))  # categories + total + base_match + time
                
            table_data.append(row)
        
        # Заголовки таблицы
        headers = ['Файл'] + category_names + ['Всего', 'База %', 'Время']
        
        if to_markdown:
            table_str = tabulate(table_data, headers=headers, tablefmt='pipe')
            self.markdown_content.append(f"{table_str}\n")
        else:
            print(tabulate(table_data, headers=headers, tablefmt='grid'))
        
        # Добавляем пояснения
        explanations = [
            "❌ - ошибка при парсинге файла",
            "База % - процент найденных объектов, существующих в базовых файлах",
            "Прочие DDIC - другие DDIC зависимости (не проверяются по базе)"
        ]
        
        if to_markdown:
            self.markdown_content.append("\n**Пояснения:**\n")
            for explanation in explanations:
                self.markdown_content.append(f"- {explanation}\n")
        else:
            print("\nПояснения:")
            for explanation in explanations:
                print(explanation)
    
    def generate_category_summary(self, to_markdown: bool = False):
        """Генерирует сводку по категориям объектов"""
        title = "СВОДКА ПО КАТЕГОРИЯМ ОБЪЕКТОВ"
        separator = "=" * 80
        
        if to_markdown:
            self.markdown_content.append(f"\n## {title}\n")
        else:
            print("\n" + separator)
            print(title)
            print(separator)
        
        categories = ['classes_and_interfaces', 'functions', 'database_tables', 'other_ddic_dependencies']
        category_names = {
            'classes_and_interfaces': 'КЛАССЫ И ИНТЕРФЕЙСЫ',
            'functions': 'ФУНКЦИИ',
            'database_tables': 'ТАБЛИЦЫ БД',
            'other_ddic_dependencies': 'ПРОЧИЕ DDIC ЗАВИСИМОСТИ'
        }
        
        for category in categories:
            category_title = category_names[category]
            
            if to_markdown:
                self.markdown_content.append(f"\n### {category_title}\n")
            else:
                print(f"\n--- {category_title} ---")
            
            # Собираем все объекты данной категории из всех файлов
            all_objects = set()
            file_objects = {}
            
            for file_path, data in self.file_results.items():
                if data['parse_result']['status'] == 'success':
                    objects = set(data['analysis']['categories'].get(category, {}).get('items', []))
                    file_objects[file_path] = objects
                    all_objects.update(objects)
            
            if not all_objects:
                no_objects_msg = "Объекты не найдены"
                if to_markdown:
                    self.markdown_content.append(f"*{no_objects_msg}*\n")
                else:
                    print(no_objects_msg)
                continue
            
            # Создаем таблицу объектов
            table_data = []
            for obj in sorted(all_objects):
                # Проверяем существование в базе
                exists_in_base = self._check_object_existence(obj, category)
                
                display_obj = obj
                if exists_in_base is not None and not exists_in_base:
                    display_obj = f"{obj} ❌"
                
                # Считаем в скольких файлах встречается
                file_count = sum(1 for objects in file_objects.values() if obj in objects)
                files_with_obj = [fp for fp, objects in file_objects.items() if obj in objects]
                
                row = [
                    display_obj,
                    file_count,
                    '✓' if exists_in_base else ('✗' if exists_in_base is not None else '-'),
                    ', '.join(files_with_obj[:3]) + ('...' if len(files_with_obj) > 3 else '')
                ]
                table_data.append(row)
            
            headers = ['Объект', 'Файлов', 'В базе', 'Файлы (первые 3)']
            
            if to_markdown:
                table_str = tabulate(table_data, headers=headers, tablefmt='pipe')
                self.markdown_content.append(f"{table_str}\n")
            else:
                print(tabulate(table_data, headers=headers, tablefmt='grid'))
            
            # Статистика по категории
            total_objects = len(all_objects)
            if category != 'other_ddic_dependencies':
                existing_objects = sum(1 for obj in all_objects if self._check_object_existence(obj, category))
                percentage = (existing_objects / total_objects * 100) if total_objects > 0 else 0
                stats_msg = f"Статистика: {total_objects} уникальных объектов, {existing_objects} найдено в базе ({percentage:.1f}%)"
            else:
                stats_msg = f"Статистика: {total_objects} уникальных объектов (проверка базы не применяется)"
            
            if to_markdown:
                self.markdown_content.append(f"\n*{stats_msg}*\n")
            else:
                print(f"\n{stats_msg}")
    
    def _create_summary_statistics(self, file_results):
        """Создает сводную статистику по всем файлам"""
        categories = ['classes_and_interfaces', 'functions', 'database_tables', 'other_ddic_dependencies']
        summary = {
            'total_objects_by_category': {},
            'unique_objects_by_category': {},
            'base_match_statistics': {},
            'overall_statistics': {
                'total_files_analyzed': len(file_results),
                'successful_files': sum(1 for data in file_results.values() if data['parse_result']['status'] == 'success'),
                'failed_files': sum(1 for data in file_results.values() if data['parse_result']['status'] != 'success'),
                'total_objects_found': 0,
                'average_objects_per_file': 0
            }
        }
        
        # Собираем статистику по категориям
        for category in categories:
            all_objects = set()
            total_count = 0
            
            for data in file_results.values():
                if data['parse_result']['status'] == 'success':
                    objects = set(data['analysis']['categories'].get(category, {}).get('items', []))
                    all_objects.update(objects)
                    total_count += len(objects)
            
            summary['total_objects_by_category'][category] = total_count
            summary['unique_objects_by_category'][category] = len(all_objects)
            
            # Статистика соответствия базе (кроме other_ddic_dependencies)
            if category != 'other_ddic_dependencies' and all_objects:
                existing_objects = sum(1 for obj in all_objects if self._check_object_existence(obj, category))
                summary['base_match_statistics'][category] = {
                    'total_unique': len(all_objects),
                    'existing_in_base': existing_objects,
                    'percentage': (existing_objects / len(all_objects) * 100) if all_objects else 0
                }
        
        # Общая статистика
        total_objects = sum(summary['total_objects_by_category'].values())
        successful_files = summary['overall_statistics']['successful_files']
        
        summary['overall_statistics']['total_objects_found'] = total_objects
        summary['overall_statistics']['average_objects_per_file'] = (total_objects / successful_files) if successful_files > 0 else 0
        
        return summary
    
    def save_results(self, output_file: str = None):
        """Сохраняет результаты в JSON файл"""
        if not output_file:
            timestamp = int(time.time())
            folder_name = os.path.basename(self.folder_path.rstrip(os.sep))
            output_file = f"folder_analysis_{folder_name}_{timestamp}.json"
        
        # Создаем сводную статистику
        summary_stats = self._create_summary_statistics(self.file_results)
            
        output_data = {
            'input_folder': self.folder_path,
            'timestamp': time.time(),
            'total_files': len(self.file_results),
            'successful_files': sum(1 for data in self.file_results.values() 
                                 if data['parse_result']['status'] == 'success'),
            'summary_statistics': summary_stats,
            'file_results': self.file_results
        }
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(output_data, f, indent=2, ensure_ascii=False)
            
        print(f"\nРезультаты сохранены в файл: {output_file}")
    
    def generate_missing_objects_report(self, to_markdown: bool = False):
        """Генерирует отчет об объектах, не найденных в базе"""
        title = "ОБЪЕКТЫ НЕ НАЙДЕННЫЕ В БАЗЕ"
        separator = "=" * 80
        
        if to_markdown:
            self.markdown_content.append(f"\n## {title}\n")
        else:
            print("\n" + separator)
            print(title)
            print(separator)
        
        categories = ['classes_and_interfaces', 'functions', 'database_tables']
        category_names = {
            'classes_and_interfaces': 'КЛАССЫ И ИНТЕРФЕЙСЫ',
            'functions': 'ФУНКЦИИ',
            'database_tables': 'ТАБЛИЦЫ БД'
        }
        
        has_missing = False
        
        for category in categories:
            missing_objects = []
            
            for file_path, data in self.file_results.items():
                if data['parse_result']['status'] == 'success':
                    objects = data['analysis']['categories'].get(category, {}).get('items', [])
                    for obj in objects:
                        if not self._check_object_existence(obj, category):
                            missing_objects.append((obj, os.path.basename(file_path)))
            
            if missing_objects:
                has_missing = True
                category_title = category_names[category]
                
                if to_markdown:
                    self.markdown_content.append(f"\n### {category_title}\n")
                else:
                    print(f"\n--- {category_title} ---")
                    
                table_data = []
                for obj, filename in sorted(set(missing_objects)):
                    # Найдем все файлы где встречается этот объект
                    files_with_obj = []
                    for fp, data in self.file_results.items():
                        if data['parse_result']['status'] == 'success':
                            if obj in data['analysis']['categories'].get(category, {}).get('items', []):
                                files_with_obj.append(os.path.basename(fp))
                    
                    table_data.append([obj, ', '.join(sorted(set(files_with_obj)))])
                
                headers = ['Объект', 'Программы']
                
                if to_markdown:
                    table_str = tabulate(table_data, headers=headers, tablefmt='pipe')
                    self.markdown_content.append(f"{table_str}\n")
                else:
                    print(tabulate(table_data, headers=headers, tablefmt='grid'))
        
        success_message = "✅ Все объекты найдены в базовых файлах!"
        if not has_missing:
            if to_markdown:
                self.markdown_content.append(f"\n*{success_message}*\n")
            else:
                print(f"\n{success_message}")
    
    def generate_full_objects_list(self, to_markdown: bool = False):
        """Генерирует полный список объектов для каждой программы"""
        title = "ПОЛНЫЙ СПИСОК ОБЪЕКТОВ ПО ПРОГРАММАМ"
        separator = "=" * 80
        
        if to_markdown:
            self.markdown_content.append(f"\n## {title}\n")
        else:
            print("\n" + separator)
            print(title)
            print(separator)
        
        categories = ['classes_and_interfaces', 'functions', 'database_tables']
        category_names = {
            'classes_and_interfaces': 'Классы и интерфейсы',
            'functions': 'Функции',
            'database_tables': 'Таблицы БД'
        }
        
        for file_path, data in self.file_results.items():
            filename = os.path.basename(file_path)
            
            if to_markdown:
                self.markdown_content.append(f"\n### ПРОГРАММА: {filename}\n")
            else:
                print(f"\n{'='*60}")
                print(f"ПРОГРАММА: {filename}")
                print(f"{'='*60}")
            
            if data['parse_result']['status'] != 'success':
                error_msg = "❌ Ошибка при парсинге файла"
                if to_markdown:
                    self.markdown_content.append(f"*{error_msg}*\n")
                else:
                    print(error_msg)
                continue
            
            total_objects = data['analysis']['total_objects']
            file_size = data['parse_result'].get('file_size', 'неизвестно')
            exec_time = data['parse_result'].get('execution_time', 0)
            
            stats = [
                f"Всего объектов: {total_objects}",
                f"Размер файла: {file_size} байт",
                f"Время парсинга: {exec_time:.3f} сек"
            ]
            
            if to_markdown:
                for stat in stats:
                    self.markdown_content.append(f"- {stat}\n")
                self.markdown_content.append("\n")
            else:
                for stat in stats:
                    print(stat)
            
            for category in categories:
                objects = data['analysis']['categories'].get(category, {}).get('items', [])
                if objects:
                    category_title = f"{category_names[category]} ({len(objects)})"
                    
                    if to_markdown:
                        self.markdown_content.append(f"\n#### {category_title}\n")
                    else:
                        print(f"\n--- {category_title} ---")
                    
                    # Группируем объекты по статусу в базе
                    in_base = []
                    not_in_base = []
                    
                    for obj in sorted(objects):
                        if self._check_object_existence(obj, category):
                            in_base.append(obj)
                        else:
                            not_in_base.append(obj)
                    
                    if in_base:
                        if to_markdown:
                            self.markdown_content.append("\n**✅ В базе:**\n")
                            for obj in in_base:
                                self.markdown_content.append(f"- {obj}\n")
                        else:
                            print("✅ В базе:")
                            for obj in in_base:
                                print(f"   • {obj}")
                    
                    if not_in_base:
                        if to_markdown:
                            self.markdown_content.append("\n**❌ Не в базе:**\n")
                            for obj in not_in_base:
                                self.markdown_content.append(f"- {obj}\n")
                        else:
                            print("❌ Не в базе:")
                            for obj in not_in_base:
                                print(f"   • {obj}")
    
    def run_full_analysis(self, save_to_file: bool = True, output_file: str = None, generate_markdown: bool = False):
        """Запускает полный анализ папки"""
        try:
            # Анализируем все файлы
            self.analyze_folder()
            
            # Генерируем сводную таблицу
            self.generate_summary_table(to_markdown=generate_markdown)
            
            # Генерируем сводку по категориям
            self.generate_category_summary(to_markdown=generate_markdown)
            
            # Генерируем отчет об объектах не найденных в базе
            self.generate_missing_objects_report(to_markdown=generate_markdown)
            
            # Генерируем полный список объектов по программам
            self.generate_full_objects_list(to_markdown=generate_markdown)
            
            # Сохраняем результаты
            if save_to_file:
                if generate_markdown:
                    self.save_markdown_report(output_file)
                else:
                    self.save_results(output_file)
                
        except Exception as e:
            print(f"Ошибка при выполнении анализа: {e}")
            raise
    
    def save_markdown_report(self, output_file: str = None):
        """Сохраняет markdown отчет в файл"""
        try:
            if not output_file:
                timestamp = int(time.time())
                folder_name = os.path.basename(self.folder_path.rstrip(os.sep))
                output_file = f"abap_analysis_{folder_name}_{timestamp}.md"
            
            # Создаем заголовок отчета
            header = [
                f"# Отчет анализа ABAP файлов\n\n",
                f"**Папка:** `{self.folder_path}`\n\n",
                f"**Дата анализа:** {time.strftime('%Y-%m-%d %H:%M:%S')}\n\n",
                f"**Количество файлов:** {len(self.file_results)}\n\n"
            ]
            
            # Объединяем заголовок и содержимое
            full_content = ''.join(header + self.markdown_content)
            
            # Сохраняем в файл
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(full_content)
            
            print(f"\n✅ Markdown отчет сохранен в файл: {output_file}")
            
        except Exception as e:
            print(f"\n❌ Ошибка при сохранении markdown отчета: {e}")


def main():
    """Главная функция"""
    if len(sys.argv) < 2:
        print("Использование: python folder_analyzer.py <путь_к_папке> [выходной_файл] [--markdown]")
        print("Пример: python folder_analyzer.py C:\\ABAP_Files results.json")
        print("Пример с markdown: python folder_analyzer.py C:\\ABAP_Files report.md --markdown")
        sys.exit(1)
        
    folder_path = sys.argv[1]
    output_file = None
    generate_markdown = False
    
    # Обрабатываем дополнительные аргументы
    for i in range(2, len(sys.argv)):
        arg = sys.argv[i]
        if arg == '--markdown':
            generate_markdown = True
        elif not arg.startswith('--'):
            output_file = arg
    
    print("ABAP FOLDER ANALYZER (Claude4Parser)")
    print("=" * 80)
    print(f"Input folder: {folder_path}")
    if output_file:
        print(f"Output file: {output_file}")
    if generate_markdown:
        print("Output format: Markdown")
    print()
    
    analyzer = FolderAnalyzer(folder_path)
    analyzer.run_full_analysis(save_to_file=True, output_file=output_file, generate_markdown=generate_markdown)
    
    print("\nАнализ папки завершен!")


if __name__ == "__main__":
    main()