#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Финальный Алгоритм (Версия 3.1): Контекстно-ориентированный гибридный анализатор ABAP
Автор: AI Assistant
Версия: 3.1
"""

import re
import json
from typing import Dict, List, Set, Tuple
from pathlib import Path


class ABAPParser:
    """Контекстно-ориентированный гибридный анализатор ABAP"""
    
    def __init__(self):
        self.literals_map: Dict[str, str] = {}
        self.literal_counter = 0
        
        # Результирующие множества
        self.database_tables: Set[str] = set()
        self.other_ddic_dependencies: Set[str] = set()
        self.functions: Set[str] = set()
        self.classes_and_interfaces: Set[str] = set()
        
        # Базовые типы ABAP для исключения
        self.basic_types = {
            'C', 'N', 'D', 'T', 'X', 'I', 'P', 'F', 'STRING', 'XSTRING',
            'DECFLOAT16', 'DECFLOAT34', 'INT1', 'INT2', 'INT4', 'INT8',
            'TABLE', 'STANDARD', 'SORTED', 'HASHED', 'INDEX', 'UNIQUE',
            'NON-UNIQUE', 'WITH', 'HEADER', 'LINE', 'OF', 'REF', 'TO'
        }
    
    def parse_file(self, file_path: str) -> Dict[str, List[str]]:
        """Основной метод парсинга файла ABAP"""
        # Этап I: Безопасная предварительная обработка и маскировка
        original_content = self._read_file(file_path)
        masked_content = self._mask_literals(original_content)
        clean_content = self._remove_comments(masked_content)
        
        line_by_line_clean_content = clean_content.split('\n')
        sql_ready_content = re.sub(r'\s+', ' ', clean_content).upper()
        
        # Поиск всех функций во всем файле (до разделения на блоки)
        self._find_all_functions(masked_content)
        
        # Этап II: Изоляция и классификация синтаксических блоков
        declaration_blocks = self._isolate_declaration_blocks(clean_content)
        sql_blocks = self._isolate_sql_blocks(clean_content, declaration_blocks)
        remaining_code = self._get_remaining_code(clean_content, declaration_blocks, sql_blocks)
        
        # Этап III: Детальный анализ изолированных блоков
        self._analyze_sql_blocks(sql_blocks)
        self._analyze_declaration_blocks(declaration_blocks)
        self._analyze_remaining_code(remaining_code)
        
        # Этап IV: Финальная сборка и вывод
        return self._generate_final_report()
    
    def _read_file(self, file_path: str) -> str:
        """Чтение файла"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
        except UnicodeDecodeError:
            # Попытка с другой кодировкой
            with open(file_path, 'r', encoding='cp1251') as f:
                return f.read()
    
    def _mask_literals(self, content: str) -> str:
        """Маскировка строковых и текстовых литералов"""
        self.literals_map.clear()
        self.literal_counter = 0
        
        # Паттерн для строковых литералов ('...' с учетом экранирования '')
        string_pattern = r"'(?:[^']|'')*'"
        
        # Паттерн для текстовых литералов (`...`)
        text_pattern = r'`[^`]*`'
        
        def replace_literal(match):
            self.literal_counter += 1
            placeholder = f'#LIT_{self.literal_counter}#'
            self.literals_map[placeholder] = match.group(0)
            return placeholder
        
        # Замена строковых литералов
        content = re.sub(string_pattern, replace_literal, content)
        
        # Замена текстовых литералов
        content = re.sub(text_pattern, replace_literal, content)
        
        return content
    
    def _remove_comments(self, content: str) -> str:
        """Удаление комментариев"""
        # Удаление комментариев в начале строки (с учетом отступов)
        return re.sub(r'^\s*\*.*$', '', content, flags=re.MULTILINE)
    
    def _isolate_declaration_blocks(self, content: str) -> List[str]:
        """Изоляция блоков объявлений"""
        declaration_keywords = [
            'DATA:', 'TYPES:', 'STATICS:', 'CONSTANTS:', 
            'FIELD-SYMBOLS:', 'PARAMETERS:', 'SELECT-OPTIONS:'
        ]
        sql_keywords = ['WITH', 'SELECT', 'UPDATE', 'INSERT', 'DELETE', 'MODIFY', 'MERGE']
        
        blocks = []
        lines = content.split('\n')
        current_block = []
        in_declaration = False
        paren_depth = 0
        
        for line in lines:
            line_upper = line.upper().strip()
            
            # Проверка начала блока объявления
            if any(line_upper.startswith(kw) for kw in declaration_keywords):
                if current_block and in_declaration:
                    blocks.append('\n'.join(current_block))
                current_block = [line]
                in_declaration = True
                paren_depth = line.count('(') - line.count(')')
                continue
            
            if in_declaration:
                # Проверяем, не начинается ли SQL-блок
                if any(line_upper.startswith(kw) for kw in sql_keywords):
                    # Завершаем текущий блок объявлений
                    blocks.append('\n'.join(current_block))
                    current_block = []
                    in_declaration = False
                    continue
                
                current_block.append(line)
                paren_depth += line.count('(') - line.count(')')
                
                # Проверка окончания блока (точка вне скобок)
                if '.' in line and paren_depth == 0:
                    blocks.append('\n'.join(current_block))
                    current_block = []
                    in_declaration = False
        
        if current_block and in_declaration:
            blocks.append('\n'.join(current_block))
        
        return blocks
    
    def _isolate_sql_blocks(self, content: str, declaration_blocks: List[str]) -> List[str]:
        """Изоляция SQL-блоков"""
        # Удаляем блоки объявлений из контента
        working_content = content
        for block in declaration_blocks:
            working_content = working_content.replace(block, '')
        
        sql_keywords = ['WITH', 'SELECT', 'UPDATE', 'INSERT', 'DELETE', 'MODIFY', 'MERGE']
        non_sql_keywords = ['CALL', 'CLASS', 'METHOD', 'FORM', 'FUNCTION', 'REPORT', 'PROGRAM']
        
        blocks = []
        lines = working_content.split('\n')
        current_block = []
        in_sql = False
        paren_depth = 0
        
        for line in lines:
            line_stripped = line.strip()
            line_upper = line_stripped.upper()
            
            # Проверка начала SQL-блока
            if any(line_upper.startswith(kw) for kw in sql_keywords):
                if current_block and in_sql:
                    blocks.append('\n'.join(current_block))
                current_block = [line]
                in_sql = True
                paren_depth = line.count('(') - line.count(')')
                continue
            
            if in_sql:
                # Проверяем, не начинается ли не-SQL блок
                if any(line_upper.startswith(kw) for kw in non_sql_keywords):
                    # Завершаем текущий SQL-блок
                    blocks.append('\n'.join(current_block))
                    current_block = []
                    in_sql = False
                    continue
                
                current_block.append(line)
                paren_depth += line.count('(') - line.count(')')
                
                # Проверка окончания блока
                if ('.' in line_stripped or 'ENDSELECT' in line_upper) and paren_depth == 0:
                    blocks.append('\n'.join(current_block))
                    current_block = []
                    in_sql = False
        
        if current_block and in_sql:
            blocks.append('\n'.join(current_block))
        
        return blocks
    
    def _get_remaining_code(self, content: str, declaration_blocks: List[str], sql_blocks: List[str]) -> str:
        """Получение оставшегося кода"""
        working_content = content
        
        # Удаляем все найденные блоки
        for block in declaration_blocks + sql_blocks:
            working_content = working_content.replace(block, '')
        
        return working_content
    
    def _analyze_sql_blocks(self, sql_blocks: List[str]):
        """Анализ SQL-блоков для поиска таблиц БД"""
        for block in sql_blocks:
            block_upper = block.upper()
            
            if 'SELECT' in block_upper or 'WITH' in block_upper:
                self._analyze_select_block(block_upper)
            else:
                self._analyze_dml_block(block_upper)
    
    def _analyze_select_block(self, block: str):
        """Анализ SELECT-блока"""
        alias_map = {}
        
        # Сначала исключаем внутренние таблицы из INTO TABLE/APPENDING TABLE
        into_table_pattern = r'INTO\s+(?:TABLE\s+|APPENDING\s+TABLE\s+)([A-Z_][A-Z0-9_]*)'
        into_table_matches = re.findall(into_table_pattern, block)
        internal_tables_in_select = set()
        
        for table_name in into_table_matches:
            if self._is_internal_table(table_name):
                internal_tables_in_select.add(table_name)
        
        # Обрабатываем CTE (Common Table Expressions)
        cte_pattern = r'WITH\s+\+([A-Z_][A-Z0-9_]*)\s+AS\s*\([^)]+SELECT[^)]+FROM\s+([A-Z_][A-Z0-9_]*)[^)]*\)'
        cte_matches = re.findall(cte_pattern, block)
        
        for cte_name, source_table in cte_matches:
            if not source_table.startswith('@') and source_table not in internal_tables_in_select:
                self.database_tables.add(source_table)
                alias_map['+' + cte_name] = source_table
        
        # Поиск таблиц в FROM
        from_pattern = r'FROM\s+([+A-Z_][A-Z0-9_]*)(?:\s+AS\s+([A-Z_][A-Z0-9_]*))?' 
        from_matches = re.findall(from_pattern, block)
        
        for table, alias in from_matches:
            if not table.startswith('@') and table not in internal_tables_in_select:
                if table.startswith('+'):
                    # CTE reference
                    if table in alias_map:
                        self.database_tables.add(alias_map[table])
                else:
                    self.database_tables.add(table)
                
                if alias:
                    alias_map[alias] = table
                else:
                    alias_map[table] = table
        
        # Поиск таблиц в JOIN
        join_pattern = r'(?:INNER\s+|LEFT\s+OUTER\s+|RIGHT\s+OUTER\s+|FULL\s+OUTER\s+|CROSS\s+)?JOIN\s+([A-Z_][A-Z0-9_]*)(?:\s+AS\s+([A-Z_][A-Z0-9_]*))?' 
        join_matches = re.findall(join_pattern, block)
        
        for match in join_matches:
            table_name = match[0]
            alias = match[1] if match[1] else table_name
            
            if not table_name.startswith('@') and table_name not in internal_tables_in_select:
                self.database_tables.add(table_name)
                alias_map[alias] = table_name
        
        # Разрешение псевдонимов в полях
        field_pattern = r'([A-Z_][A-Z0-9_]*)~[A-Z_][A-Z0-9_]*'
        field_matches = re.findall(field_pattern, block)
        
        for alias in field_matches:
            if alias in alias_map:
                real_table = alias_map[alias]
                if real_table.startswith('+') and real_table in alias_map:
                    self.database_tables.add(alias_map[real_table])
                else:
                    self.database_tables.add(real_table)
    
    def _analyze_dml_block(self, block: str):
        """Анализ DML-блоков (UPDATE, INSERT, DELETE, MODIFY, MERGE)"""
        # Специальная обработка MODIFY - исключаем операции с внутренними таблицами
        modify_pattern = r'MODIFY\s+([A-Z_][A-Z0-9_]*)(?:\s+FROM|\s+SET)'
        modify_matches = re.findall(modify_pattern, block)
        
        for match in modify_matches:
            # Проверяем, что это не операция с внутренней таблицей
            # Также исключаем MODIFY ... FROM ... (это операция с внутренними таблицами)
            if (not match.startswith('@') and 'INDEX' not in block and 
                'FROM' not in block and not self._is_internal_table(match)):
                self.database_tables.add(match)
        
        # Специальная обработка DELETE - исключаем операции с внутренними таблицами
        delete_pattern = r'DELETE\s+(?:FROM\s+)?([A-Z_][A-Z0-9_]*)(?:\s+WHERE|\s*\.)'
        delete_matches = re.findall(delete_pattern, block)
        
        for match in delete_matches:
            # Проверяем, что это не операция с внутренней таблицей (DELETE ADJACENT DUPLICATES FROM)
            if not match.startswith('@') and 'ADJACENT' not in block and not self._is_internal_table(match):
                self.database_tables.add(match)
        
        # Обработка остальных DML-операций (UPDATE, INSERT, MERGE)
        patterns = [
            r'UPDATE\s+([A-Z_][A-Z0-9_]*)(?:\s+SET|\s+FROM)',
            r'INSERT\s+(?:INTO\s+)?([A-Z_][A-Z0-9_]*)(?:\s+FROM|\s+VALUES)',
            r'MERGE\s+INTO\s+([A-Z_][A-Z0-9_]*)'
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, block)
            for match in matches:
                if not match.startswith('@'):
                    self.database_tables.add(match)
    
    def _analyze_declaration_blocks(self, declaration_blocks: List[str]):
        """Анализ блоков объявлений для поиска DDIC-зависимостей"""
        for block in declaration_blocks:
            block_upper = block.upper()
            
            # Поиск TYPE REF TO (сначала, чтобы не конфликтовать с обычным TYPE)
            ref_pattern = r'TYPE\s+REF\s+TO\s+([A-Z_][A-Z0-9_]*)'
            ref_matches = re.findall(ref_pattern, block_upper)
            for match in ref_matches:
                if self._is_class_or_interface(match):
                    self.classes_and_interfaces.add(match)
            
            # Поиск TABLE OF
            table_of_pattern = r'TYPE\s+(?:STANDARD\s+|SORTED\s+|HASHED\s+)?TABLE\s+OF\s+([A-Z_][A-Z0-9_]*)'
            table_of_matches = re.findall(table_of_pattern, block_upper)
            for match in table_of_matches:
                if self._is_ddic_object(match):
                    self.other_ddic_dependencies.add(match)
            
            # Поиск обычных TYPE (исключая уже обработанные REF TO и TABLE OF)
            # Убираем из блока уже обработанные конструкции
            temp_block = re.sub(r'TYPE\s+REF\s+TO\s+[A-Z_][A-Z0-9_]*', '', block_upper)
            temp_block = re.sub(r'TYPE\s+(?:STANDARD\s+|SORTED\s+|HASHED\s+)?TABLE\s+OF\s+[A-Z_][A-Z0-9_]*', '', temp_block)
            
            # Улучшенные паттерны для TYPE и LIKE
            type_pattern = r'(?:^|\s|,)\w+\s+TYPE\s+([A-Z_][A-Z0-9_]*)(?:\s|,|\.|$)'
            type_matches = re.findall(type_pattern, temp_block)
            
            # Поиск LIKE
            like_pattern = r'(?:^|\s|,)\w+\s+LIKE\s+([A-Z_][A-Z0-9_]*)(?:\s|,|\.|$)'
            like_matches = re.findall(like_pattern, block_upper)
            
            for match in type_matches + like_matches:
                if self._is_ddic_object(match):
                    self.other_ddic_dependencies.add(match)
    
    def _analyze_remaining_code(self, remaining_code: str):
        """Анализ оставшегося кода для поиска классов и интерфейсов"""
        remaining_upper = remaining_code.upper()
        
        # Поиск наследования классов
        inherit_pattern = r'CLASS\s+([A-Z_][A-Z0-9_]*)\s+(?:DEFINITION\s+)?INHERITING\s+FROM\s+([A-Z_][A-Z0-9_]*)'
        inherit_matches = re.findall(inherit_pattern, remaining_upper)
        for match in inherit_matches:
            # Добавляем только родительский класс, исключаем DEFINITION
            if match[1] != 'DEFINITION' and self._is_class_or_interface(match[1]):
                self.classes_and_interfaces.add(match[1])
        
        # Поиск интерфейсов
        interface_pattern = r'INTERFACES\s*:\s*([A-Z_][A-Z0-9_]*(?:\s*,\s*[A-Z_][A-Z0-9_]*)*)'
        interface_matches = re.findall(interface_pattern, remaining_upper)
        for match in interface_matches:
            # Разбираем список интерфейсов через запятую
            interfaces = re.findall(r'([A-Z_][A-Z0-9_]*)', match)
            for interface in interfaces:
                if self._is_class_or_interface(interface):
                    self.classes_and_interfaces.add(interface)
        
        # Простой паттерн для INTERFACES без двоеточия
        simple_interface_pattern = r'INTERFACES\s+([A-Z_][A-Z0-9_]*)'
        simple_interface_matches = re.findall(simple_interface_pattern, remaining_upper)
        for match in simple_interface_matches:
            if self._is_class_or_interface(match):
                self.classes_and_interfaces.add(match)
        
        # Поиск TYPE REF TO
        ref_pattern = r'TYPE\s+REF\s+TO\s+([A-Z_][A-Z0-9_]*)'
        ref_matches = re.findall(ref_pattern, remaining_upper)
        for match in ref_matches:
            if self._is_class_or_interface(match):
                self.classes_and_interfaces.add(match)
        
        # Поиск вызовов методов классов
        method_pattern = r'([A-Z_][A-Z0-9_]*)=>'
        method_matches = re.findall(method_pattern, remaining_upper)
        for match in method_matches:
            if self._is_class_or_interface(match):
                self.classes_and_interfaces.add(match)
    
    def _is_ddic_object(self, name: str) -> bool:
        """Проверка, является ли имя DDIC-объектом"""
        if name in self.basic_types:
            return False
        
        # Исключаем служебные слова
        excluded_words = {
            'VALUE', 'DEFAULT', 'INITIAL', 'LENGTH', 'DECIMALS', 'REF', 'TO',
            'TYPE', 'LIKE', 'BEGIN', 'END', 'OCCURS', 'INCLUDE', 'AS',
            'STRUCTURE', 'RANGE', 'SIGN', 'OPTION', 'LOW', 'HIGH'
        }
        if name in excluded_words:
            return False
        
        # Исключаем переменные по префиксам (включая венгерскую нотацию)
        if name.startswith(('LV_', 'LT_', 'LS_', 'LR_', 'LC_', 'WA_', 'IT_', 'I')):
            return False
        
        # Исключаем очень короткие имена (вероятно, служебные)
        if len(name) <= 1:
            return False
        
        return True
    
    def _is_internal_table(self, name: str) -> bool:
        """Проверка, является ли имя внутренней таблицей по венгерской нотации"""
        # Венгерская нотация для внутренних таблиц
        name_upper = name.upper()
        return (name_upper.startswith(('I', 'IT_', 'LT_', 'T_')) or 
                name_upper.endswith('_TAB') or
                name_upper.endswith('_R') or
                name_upper.endswith('_N') or
                name_upper.endswith('_S') or
                name_upper in ('IFLOW', 'ILOCCLASSES', 'ILOCFUNCTIONNAMES', 'ITEXTTABLE', 'ITOKENS', 
                               'FIELDCAT', 'DD02L_TAB', 'T_IDOC_CONTROL_R', 'T_IDOC_CONTROL_N', 
                               'T_IDOC_CONTROL_S', 'T_IDOC_CONTROL_TMP', 'T_PACKET', 'T_TEDS2', 
                               'T_UPDATE', 'T_IDOC_DATA'))
    
    def _is_class_or_interface(self, name: str) -> bool:
        """Проверка, является ли имя классом или интерфейсом"""
        # Исключаем служебные слова
        excluded_words = {'DEFINITION', 'IMPLEMENTATION', 'PUBLIC', 'PRIVATE', 'PROTECTED'}
        if name in excluded_words:
            return False
        
        # Исключаем стандартные интерфейсы начинающиеся на IF_
        if name.startswith('IF_'):
            return False
            
        return (name.startswith(('ZCL_', 'YCL_', 'CL_')) or 
                name.startswith(('ZIF_', 'YIF_')) or 
                name.startswith(('ZCX_', 'YCX_', 'CX_')))
    
    def _find_all_functions(self, content: str):
        """Универсальный поиск всех функций во всем исходном коде"""
        # Восстанавливаем литералы для поиска функций
        content_with_literals = content
        for placeholder, original in self.literals_map.items():
            content_with_literals = content_with_literals.replace(placeholder, original)
        
        # Паттерн для CALL FUNCTION
        function_pattern = r"CALL\s+FUNCTION\s+'([^']+)'"
        function_matches = re.findall(function_pattern, content_with_literals, re.IGNORECASE | re.MULTILINE | re.DOTALL)
        self.functions.update(function_matches)
    
    def _generate_final_report(self) -> Dict[str, List[str]]:
        """Генерация финального отчета"""
        return {
            'classes_and_interfaces': sorted(list(self.classes_and_interfaces)),
            'functions': sorted(list(self.functions)),
            'database_tables': sorted(list(self.database_tables)),
            'other_ddic_dependencies': sorted(list(self.other_ddic_dependencies))
        }


def main():
    """Основная функция для тестирования парсера"""
    import sys
    
    if len(sys.argv) != 2:
        print("Использование: python abap_parser.py <путь_к_файлу_abap>")
        sys.exit(1)
    
    file_path = sys.argv[1]
    
    if not Path(file_path).exists():
        print(f"Файл не найден: {file_path}")
        sys.exit(1)
    
    parser = ABAPParser()
    result = parser.parse_file(file_path)
    
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == '__main__':
    main()